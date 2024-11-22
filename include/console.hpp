#ifndef MS_CONSOLE_HPP
#define MS_CONSOLE_HPP

#include <string>
#include <vector>
#include <algorithm>
#include <iostream>

#include "env.hpp"

namespace ms::console {

    inline bool startsw(const char* s, char c) {
        return s && (*s) == c;
    }

    std::string str(const char* s, int o) {
        return std::string(s).substr(o);
    }

    struct Option {

        std::string name;
        char symbol {'\0'};

        bool reqval;
        bool isset;

        std::string value;

    };

    struct Options {

        std::vector<Option> options;

        Options() {}
        Options(std::initializer_list<Option> opts) : options(opts) {}

        bool isset(std::string_view name) const {
            auto opt = std::find_if(options.begin(), options.end(),
                [&](const auto& o) { return o.name == name; });

            return opt == options.end() ? false : opt->isset;
        }

    };

    struct ConsoleOptions {

        static int error(int status, const char* msg) {
            std::cerr << msg << '\n';

            return status;
        }

        static int error(int status, const char* msg, std::string_view arg) {
            std::cerr << msg << ' ' << arg << '\n';

            return status;
        }

        template <int N>
        static int read2(const Option (&options)[N], std::vector<std::string>& args, int argc, char** argv) {
            std::vector<Option> f { options };
        }

        static int read(Options& opts, std::vector<std::string>& args, const int argc, char** argv) {
            Option* chkop {nullptr};

            for (int i = 0; i < argc; i++) {
                char* arg = argv[i];

                /* set value of option */
                if (chkop) {
                    if (!arg || *arg == '\0') {
                        /* ERROR: required argument of param */
                        return error(-1, "Error: missing argument of option:", chkop->name);
                    }

                    chkop->value = arg;
                    chkop = nullptr;

                    continue;
                }

                if (arg && *arg == '-') {
                    /* long names */
                    if (*(arg + 1) == '-') {
                        const auto param { str(arg, 2) };
                        const auto opt = std::find_if(opts.options.begin(), opts.options.end(),
                            [&](const auto& o) { return o.name == param; });

                        if (opt == opts.options.end()) {
                            /* ERROR: unknown option */
                            return error(-1, "Error: unknown option:", param);
                        }

                        if (opt->reqval) {
                            if (chkop)
                                return error(-1, "Error: another option is waiting for a parameter:", chkop->name);
                            
                            chkop = opt.base();
                        }

                        opt->isset = true;
                    }

                    /* symbols (-abc => a,b,c) */
                    char* symbol = arg + 1;

                    while (*symbol != '\0') {
                        const auto opt = std::find_if(opts.options.begin(), opts.options.end(),
                            [&](const auto& o) { return o.symbol == *symbol; });

                        if (opt == opts.options.end()) {
                            /* ERROR: unknown option */
                            return error(-1, "Error: unknown option:", std::string(1, *symbol));
                        }

                        if (opt->reqval) {
                            if (chkop)
                                return error(-1, "Error: another option is waiting for a parameter:", chkop->name);

                            chkop = opt.base();
                        }

                        opt->isset = true;
                        ++symbol;
                    }

                    continue;
                }

                /* ARG... */
                if (arg) {
                    args.push_back(arg);
                }
            }

            /* ERROR: option still waiting for value */
            if (chkop) {
                return error(-1, "Error: missing argument of param");
            }

            return 1;
        }

    };

    /*
     ./program [PARAMS...] ARGS...
     ./program -c -x 20 ARG0
     */

}

#endif