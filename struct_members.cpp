#include <algorithm>
#include <assert.h>
#include <cppast/code_generator.hpp>         // for generate_code()
#include <cppast/cpp_entity_kind.hpp>        // for the cpp_entity_kind definition
#include <cppast/cpp_forward_declarable.hpp> // for is_definition()
#include <cppast/cpp_namespace.hpp>          // for cpp_namespace
#include <cppast/libclang_parser.hpp>        // for libclang_parser, libclang_compile_config, cpp_entity,...
#include <cppast/visitor.hpp>                // for visit()
#include <cxxopts.hpp>
#include <fmt/format.h>
#include <fmt/ranges.h>
#include <iostream>
#include <map>

#define REFLECT_ATTRIBUTE "reflectable"
#define OUTPUT_PATTERN "{}\n"                                                     \
                       "struct __class__<{}{}{}>{{{{\n"                           \
                       "    static constexpr const char *dir[] = {{{{{{}}}}}};\n" \
                       "    auto get({}{}{} &obj, std::string name) {{{{\n"       \
                       "{{}}"                                                     \
                       "        throw std::runtime_error(name);\n"                \
                       "    }}}}\n"                                               \
                       "    template<template<typename> class callback_t>\n"      \
                       "    void get({}{}{} &obj, std::string name) {{{{\n"       \
                       "{{}}"                                                     \
                       "        throw std::runtime_error(name);\n"                \
                       "    }}}}\n"                                               \
                       "}}}};\n"

struct struct_members {
    std::string prefix;
    std::string template_arg_list; // template<typename T, int N, typename ...Vs>
    // std::string template_arg_list_specialized; // struct_name<template_arg_list_specialized>
    std::vector<std::string> member_variable_names;
    std::vector<std::string> member_function_names;
    std::string output;
};

std::ostream &operator<<(std::ostream &os, struct_members s) {
    os << fmt::format(
        "prefix:{}\n"
        "template_arg_list:{}\n"
        "member_variable_names:{}\n"
        "member_function_names:{}\n",
        s.prefix,
        s.template_arg_list,
        fmt::join(s.member_variable_names, ", "),
        fmt::join(s.member_function_names, ", "));
    return os;
}

using struct_category = std::map<std::string, struct_members>;

std::map<std::string, std::map<std::string, struct_category>> struct_list;

// struct_list[struct_name][template_arg_list_specialized] = struct_entity
// eg: some_struct <T>
// 如果是类定义本身
// template_arg_list
// std::any struct_members<struct_entity.prefix::struct_name template_arg_list_specialized>::get_member();
// 此时template_arg_list_specialized其实是去除了typename等东西的stripped arg list
// 如果是特化
// template_arg_list
// std::any struct_members<namespace::struct_name<template_arg_list_specialized>>::get_member();
// 暂时不支持在模板类中处理模板类

std::vector<std::string> prefix_list;

std::string current_class;
std::string current_pattern;
#define current_prefix (prefix_list.back())

struct code_generator_class_template : public cppast::code_generator {
    std::string template_arg_list;             // the result
    std::string template_arg_list_specialized; // stripped arg list
    std::string original_tokens;

    using state = enum {
        startup,
        wait_for_typename,
        wait_for_class,
        wait_for_template,
        wait_for_non_type,
        wait_for_next,
        wait_for_name,
        shutdown
    };
    state s{startup};
    std::string tri_dot_cache;

    code_generator_class_template(const cppast::cpp_entity &e) { cppast::generate_code(*this, e); }

    generation_options do_get_options(const cppast::cpp_entity &, cppast::cpp_access_specifier_kind) override {
        return code_generator::declaration;
    }

    // no need to handle indentation, as only a single line is used
    void do_indent() override {}
    void do_unindent() override {}

    void do_write_token_seq(cppast::string_view tokens) override {
        auto equal = [&](const char *string) {
            return std::string(string) == tokens.c_str();
        };
        switch (s) {
        case startup:
            template_arg_list += tokens.c_str();
            if (equal("<")) {
                s = wait_for_next;
                template_arg_list_specialized += tokens.c_str();
            }
            break;
        case wait_for_next:
            if (equal("typename")) {
                s = wait_for_typename;
                template_arg_list += tokens.c_str();
            } else if (equal("class")) {
                s = wait_for_class;
                template_arg_list += tokens.c_str();
            } else if (equal("template")) {
                s = wait_for_template;
                template_arg_list += tokens.c_str();
            } else {
                s = wait_for_non_type;
                template_arg_list += "auto";
            }
            break;
        case wait_for_typename:
            template_arg_list += tokens.c_str();
            if (equal("...")) {
                s = wait_for_name;
                break;
            }
            template_arg_list_specialized += tokens.c_str();
            if (equal(",")) {
                s = wait_for_next;
            } else if (equal(">")) {
                s = shutdown;
            }
            break;
        case wait_for_class:
            template_arg_list += tokens.c_str();
            if (equal("...")) {
                s = wait_for_name;
                break;
            }
            template_arg_list_specialized += tokens.c_str();
            if (equal(",")) {
                s = wait_for_next;
            } else if (equal(">")) {
                s = shutdown;
            }
            break;
        case wait_for_template:
            template_arg_list += tokens.c_str();
            if (equal("...")) {
                s = wait_for_name;
                break;
            }
            template_arg_list_specialized += tokens.c_str();
            if (equal(",")) {
                s = wait_for_next;
            } else if (equal(">")) {
                s = shutdown;
            }
            break;
        case wait_for_non_type:
            template_arg_list += tokens.c_str();
            if (equal("...")) {
                s = wait_for_name;
                break;
            }
            template_arg_list_specialized += tokens.c_str();
            if (equal(",")) {
                s = wait_for_next;
            } else if (equal(">")) {
                s = shutdown;
            }
            break;
        case wait_for_name:
            template_arg_list += tokens.c_str();
            if (equal(",")) {
                s = wait_for_next;
                template_arg_list_specialized += tri_dot_cache;
                template_arg_list_specialized += std::string("...,");
                tri_dot_cache = "";
            } else if (equal(">")) {
                s = shutdown;
                template_arg_list_specialized += tri_dot_cache;
                template_arg_list_specialized += std::string("...>");
                tri_dot_cache = "";
            } else {
                tri_dot_cache += tokens.c_str();
            }
        case shutdown:
            break;
        default:
            template_arg_list_specialized += tokens.c_str();
            break;
        }
    }
    void do_write_newline() override {}
};
struct code_generator_class_template_specialization : public cppast::code_generator {
    std::string template_arg_list;             // the result
    std::string template_arg_list_specialized; // stripped arg list
    std::string original_tokens;

    using state = enum {
        accept_template_arg,
        wait_for_template_arg_specialized,
        accept_template_arg_specialized,
        shutdown
    };
    state s{accept_template_arg};
    code_generator_class_template_specialization(const cppast::cpp_entity &e) { cppast::generate_code(*this, e); }

    generation_options do_get_options(const cppast::cpp_entity &, cppast::cpp_access_specifier_kind) override {
        return code_generator::declaration;
    }

    // no need to handle indentation, as only a single line is used
    void do_indent() override {}
    void do_unindent() override {}

    void do_write_token_seq(cppast::string_view tokens) override {

        switch (s) {
        case accept_template_arg:
            template_arg_list += tokens.c_str();
            if (std::string(">") == tokens.c_str()) {
                s = wait_for_template_arg_specialized;
            }
            break;
        case wait_for_template_arg_specialized:
            if (std::string("<") == tokens.c_str()) {
                s = accept_template_arg_specialized;
                template_arg_list_specialized += tokens.c_str();
            }
            break;
        case accept_template_arg_specialized:
            template_arg_list_specialized += tokens.c_str();
            if (std::string(">") == tokens.c_str()) {
                s = shutdown;
            }
            break;
        default:
            break;
        }

        original_tokens += tokens.c_str();
        original_tokens += "/";
    }
    void do_write_newline() override {}
};

void on_enter_container(std::ostream &out, const cppast::cpp_entity &e) {
    std::string template_arg_list_already_specialized;
    struct_members current;
    switch (e.kind()) {
    case cppast::cpp_entity_kind::class_t:
        template_arg_list_already_specialized = "";
        current.prefix                        = prefix_list.back(),
        current.template_arg_list             = "template<>";
        break;
    case cppast::cpp_entity_kind::class_template_t: {
        code_generator_class_template generator(e);
        template_arg_list_already_specialized = generator.template_arg_list_specialized;
        current.prefix                        = prefix_list.back();
        current.template_arg_list             = generator.template_arg_list;
    } break;
    case cppast::cpp_entity_kind::class_template_specialization_t: {
        code_generator_class_template_specialization generator(e);
        template_arg_list_already_specialized = generator.template_arg_list_specialized;
        current.prefix                        = prefix_list.back();
        current.template_arg_list             = generator.template_arg_list;
    } break;
    case cppast::cpp_entity_kind::namespace_t:
        // out << "enter(kind=" << int(e.kind()) << "):" << current_prefix << e.name() << "\t" << code_generator(e).str() << "\n";
        prefix_list.push_back(prefix_list.back() + e.name() + "::");
        return;
    default:
        return;
    }
    // out << "enter(kind=" << int(e.kind()) << "):" << current_prefix << e.name() << "\t" << code_generator(e).str() << "\n";
    current.output  = fmt::format(FMT_STRING(OUTPUT_PATTERN),
                                 current.template_arg_list,
                                 current.prefix,
                                 e.name(),
                                 template_arg_list_already_specialized,
                                 current.prefix,
                                 e.name(),
                                 template_arg_list_already_specialized,
                                 current.prefix,
                                 e.name(),
                                 template_arg_list_already_specialized);
    current_class   = e.name();
    current_pattern = template_arg_list_already_specialized;

    struct_list[current_prefix][current_class][current_pattern] = current;

    // out << fmt::format("struct_list[{}][{}][{}]\n", current_prefix, e.name(), template_arg_list_already_specialized);
    // out << struct_list[current_prefix][e.name()][template_arg_list_already_specialized].output;

    prefix_list.push_back(prefix_list.back() + e.name() + "::");
    current_prefix = prefix_list.back();
}
void on_exit_container(std::ostream &out, const cppast::cpp_entity &e) {
    if (((e.kind() == cppast::cpp_entity_kind::class_t) or
         (e.kind() == cppast::cpp_entity_kind::class_template_t) or
         (e.kind() == cppast::cpp_entity_kind::class_template_specialization_t))) {
        prefix_list.pop_back();
        if (!cppast::has_attribute(e, REFLECT_ATTRIBUTE)) { return; }
        const struct_members &current = struct_list[current_prefix][current_class][current_pattern];
        std::string ifexpr;
        std::string getexpr;
        for (auto &v : current.member_variable_names) {
            ifexpr += fmt::format("        if (name == \"{}\") {{return std::any(obj.{});}}\n", v, v);
            getexpr += fmt::format("        if (name == \"{}\") {{callback_t<decltype(obj.{})>::get(&obj.{});return;}}\n", v, v, v);
        }
        out << fmt::format("// struct_list[{}][{}][{}]\n", current_prefix, e.name(), current_pattern);
        out << fmt::format(
            current.output,
            current.member_variable_names.size() > 0 ? fmt::format("\"{}\"", fmt::join(current.member_variable_names, "\", \"")) : std::string(""),
            ifexpr,
            getexpr);
        return;
        // out << "exit (kind=" << int(e.kind()) << "):" << current_prefix << e.name() << "\t" << code_generator(e).str() << "\n";
    }
    if (e.kind() == cppast::cpp_entity_kind::namespace_t) {
        // out << "exit (kind=" << int(e.kind()) << "):" << current_prefix << e.name() << "\t" << code_generator(e).str() << "\n";
        prefix_list.pop_back();
    }
}
void on_leaf_entity(std::ostream &out, const cppast::cpp_entity &e) {
    if (e.kind() == cppast::cpp_entity_kind::member_variable_t) {
        out << fmt::format("// struct_list[{}][{}][{}]::{}\n", prefix_list.at(prefix_list.size() - 2), current_class, current_pattern, e.name());
        struct_list[prefix_list.at(prefix_list.size() - 2)][current_class][current_pattern].member_variable_names.push_back(e.name());
    }
}
void read_ast(std::ostream &out, const cppast::cpp_file &file) {
    prefix_list.push_back("");
    cppast::visit(file, [&](const cppast::cpp_entity &e, cppast::visitor_info info) {
        if (cppast::is_templated(e) || cppast::is_friended(e)) {
            return true;
        }
        if (info.event == cppast::visitor_info::container_entity_enter) {
            on_enter_container(out, e);
        }
        if (info.event == cppast::visitor_info::container_entity_exit) {
            on_exit_container(out, e);
        }
        if (info.event == cppast::visitor_info::leaf_entity) {
            on_leaf_entity(out, e);
        }
        return true;
    });
}

std::unique_ptr<cppast::cpp_file> parse_file(const cppast::libclang_compile_config &config,
                                             const cppast::diagnostic_logger &logger,
                                             const std::string &filename, bool fatal_error) {
    // the entity index is used to resolve cross references in the AST
    // we don't need that, so it will not be needed afterwards
    cppast::cpp_entity_index idx;
    // the parser is used to parse the entity
    // there can be multiple parser implementations
    cppast::libclang_parser parser(type_safe::ref(logger));
    // parse the file
    auto file = parser.parse(idx, filename, config);
    if (fatal_error && parser.error())
        return nullptr;
    return file;
}
int main(int argc, char *argv[]) {
    try {
        cxxopts::Options option_list("struct::members",
                                     "struct::members - The preprocess tool to generate static reflection information.\n");
        // clang-format off
        option_list.add_options()
        ("h,help", "display this help and exit")
        ("file", "the file that is being parsed (last positional argument)",
        cxxopts::value<std::string>());
        option_list.add_options("compilation")
        ("database_dir", "set the directory where a 'compile_commands.json' file is located containing build information",
        cxxopts::value<std::string>())
        ("database_file", "set the file name whose configuration will be used regardless of the current file name",
        cxxopts::value<std::string>())
        ("std", "set the C++ standard (c++98, c++03, c++11, c++14, c++1z (experimental))",
         cxxopts::value<std::string>()->default_value(cppast::to_string(cppast::cpp_standard::cpp_latest)))
        ("I,include_directory", "add directory to include search path",
         cxxopts::value<std::vector<std::string>>())
        ("D,macro_definition", "define a macro on the command line",
         cxxopts::value<std::vector<std::string>>())
        ("U,macro_undefinition", "undefine a macro on the command line",
         cxxopts::value<std::vector<std::string>>())
        ("f,feature", "enable a custom feature (-fXX flag)",
         cxxopts::value<std::vector<std::string>>())
        ("gnu_extensions", "enable GNU extensions (equivalent to -std=gnu++XX)")
        ("msvc_extensions", "enable MSVC extensions (equivalent to -fms-extensions)")
        ("msvc_compatibility", "enable MSVC compatibility (equivalent to -fms-compatibility)")
        ("fast_preprocessing", "enable fast preprocessing, be careful, this breaks if you e.g. redefine macros in the same file!")
        ("remove_comments_in_macro", "whether or not comments generated by macro are kept, enable if you run into errors");
        // clang-format on
        option_list.parse_positional("file");

        auto options = option_list.parse(argc, argv);
        if (!options.count("file") || options["file"].as<std::string>().empty()) {
            std::cerr << ("missing file argument\n");
            return 1;
        }
        // the compile config stores compilation flags
        cppast::libclang_compile_config config;
        if (options.count("database_dir")) {
            cppast::libclang_compilation_database database(options["database_dir"].as<std::string>());
            if (options.count("database_file")) {
                config = cppast::libclang_compile_config(database, options["database_file"].as<std::string>());
            } else {
                config = cppast::libclang_compile_config(database, options["file"].as<std::string>());
            }
        }
        if (options.count("verbose")) {
            config.write_preprocessed(true);
        }
        if (options.count("fast_preprocessing")) {
            config.fast_preprocessing(true);
        }
        if (options.count("remove_comments_in_macro")) {
            config.remove_comments_in_macro(true);
        }
        if (options.count("include_directory")) {
            for (auto &include : options["include_directory"].as<std::vector<std::string>>()) {
                config.add_include_dir(include);
            }
        }
        if (options.count("macro_definition"))
            for (auto &macro : options["macro_definition"].as<std::vector<std::string>>()) {
                auto equal = macro.find('=');
                auto name  = macro.substr(0, equal);
                if (equal == std::string::npos)
                    config.define_macro(std::move(name), "");
                else {
                    auto def = macro.substr(equal + 1u);
                    config.define_macro(std::move(name), std::move(def));
                }
            }
        if (options.count("macro_undefinition")) {
            for (auto &name : options["macro_undefinition"].as<std::vector<std::string>>()) {
                config.undefine_macro(name);
            }
        }
        if (options.count("feature")) {
            for (auto &name : options["feature"].as<std::vector<std::string>>()) {
                config.enable_feature(name);
            }
        }

        // the compile_flags are generic flags
        cppast::compile_flags flags;
        if (options.count("gnu_extensions")) {
            flags |= cppast::compile_flag::gnu_extensions;
        }
        if (options.count("msvc_extensions")) {
            flags |= cppast::compile_flag::ms_extensions;
        }
        if (options.count("msvc_compatibility")) {
            flags |= cppast::compile_flag::ms_compatibility;
        }

        if (options["std"].as<std::string>() == "c++98") {
            config.set_flags(cppast::cpp_standard::cpp_98, flags);
        } else if (options["std"].as<std::string>() == "c++03") {
            config.set_flags(cppast::cpp_standard::cpp_03, flags);
        } else if (options["std"].as<std::string>() == "c++11") {
            config.set_flags(cppast::cpp_standard::cpp_11, flags);
        } else if (options["std"].as<std::string>() == "c++14") {
            config.set_flags(cppast::cpp_standard::cpp_14, flags);
        } else if (options["std"].as<std::string>() == "c++17") {
            config.set_flags(cppast::cpp_standard::cpp_17, flags);
        } else if (options["std"].as<std::string>() == "c++20") {
            config.set_flags(cppast::cpp_standard::cpp_20, flags);
        } else {
            std::cerr << ("invalid value '" + options["std"].as<std::string>() + "' for std flag") << std::endl;
            return 1;
        }
        // the logger is used to print diagnostics
        cppast::stderr_diagnostic_logger logger;
        if (options.count("verbose")) {
            logger.set_verbose(true);
        }

        auto file = parse_file(config, logger, options["file"].as<std::string>(),
                               options.count("fatal_errors") == 1);
        if (!file) {
            return 2;
        }
        read_ast(std::cout, *file);
    } catch (const cppast::libclang_error &ex) {
        std::cerr << (std::string("[fatal parsing error] ") + ex.what()) << std::endl;
        return 2;
    }
}