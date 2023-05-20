#ifndef MS_DEBUG_HPP
#define MS_DEBUG_HPP

#include <iostream>
#include <sstream>
#include <string>
#include <typeinfo>
#include <utility>

#define MS_IF_DEBUG if (true)
#define ms_is_debug true

namespace ms {

    template <typename T, typename = void>
    struct is_streamable : std::false_type {};

    // see: is_stream_writable
    // https://stackoverflow.com/questions/22758291/how-can-i-detect-if-a-type-can-be-streamed-to-an-stdostream
    template <typename T>
    struct is_streamable<T, std::void_t<decltype(std::declval<std::ostream&>() << std::declval<T>())>> : std::true_type {};

    /* Generic handler for T -> string transformations.
     * For any type to be printed using sformat, a realisation of this
     * struct for the desired type must be defined.
     */
    template <typename T>
    struct SFmt {

        std::string toString(const T& x) {
            if constexpr (is_streamable<T>::value) {
                std::stringstream buf;

                buf << x;

                return buf.str();
            }

            return "{" + std::string(typeid(x).name()) + "}";
        }

    };

    // Standard types

    template <>
    struct SFmt<std::string> {
        std::string toString(const std::string& s) { return s; }
    };

    template <>
    struct SFmt<char*> {
        std::string toString(char* s) { return ":+" + std::string(s); }
    };

    template <>
    struct SFmt<const char*> {
        std::string toString(const char* s) { return ":-" + std::string(s); }
    };

}

namespace ms::debug {

    namespace Console {

        enum Color : int {
            FG_BLACK    = 30,
            FG_RED      = 31,
            FG_GREEN    = 32,
            FG_YELLOW   = 33,
            FG_BLUE     = 34,
            FG_MAGENTA  = 35,
            FG_CYAN     = 36,
            FG_WHITE    = 37,
            // 38
            FG_DEFAULT  = 39,

            BG_BLACK    = 40,
            BG_RED      = 41,
            BG_GREEN    = 42,
            BG_YELLOW   = 43,
            BG_BLUE     = 44,
            BG_MAGENTA  = 45,
            BG_CYAN     = 46,
            BG_WHITE    = 47,
            // 48
            BG_DEFAULT  = 49
        };

        enum Style {
            RESET       = 0,
            BOLD        = 1,
            ITALIC      = 3,
            UNDERLINED  = 4,
            INVERSED    = 7,
            BOLD_OFF    = 21,
            ITALIC_OFF  = 23,
            UNDERLINE_OFF   = 24,
            INVERSE_OFF     = 27,
            FRAMED      = 51,
            DOUBLE_LINE = 61
        };

        class Modifier {
            Color fg;
            Color bg;
            Style style;
            int type;

            public:
                Modifier(Color fg_, Color bg_, Style style_) : fg(fg_), bg(bg_), style(style_), type(4) {}
                Modifier(Color fg_, Color bg_) : fg(fg_), bg(bg_), type(3) {}
                Modifier(Color fg_, Style style_) : fg(fg_), style(style_), type(2) {}
                Modifier(Color fg_) : fg(fg_) , type(1) {}
                Modifier(Style style_) : style(style_) , type(0) {}
                Modifier() {}

                friend inline std::ostream&
                operator<<(std::ostream& os, const Modifier& mod) {
                    if (mod.type == 4)
                        return os << "\033[" << mod.fg << ";" << mod.bg << ";" << mod.style << "m";
                    else if (mod.type == 3)
                        return os << "\033[" << mod.fg << ";" << mod.bg << "m";
                    else if (mod.type == 2)
                        return os << "\033[" << mod.fg << ";" << mod.style << "m";
                    else if (mod.type == 1)
                        return os << "\033[" << mod.fg << "m";
                    else if (mod.type == 0)
                        return os << "\033[" << mod.style << "m";
                    else return os;
                }

        };

        // -- direct color output
        inline std::ostream& operator<<(std::ostream& os, Color color) {
            return os << "\033[" << static_cast<int>(color) << "m";
        }

        // -- direct style output
        inline std::ostream& operator<<(std::ostream& os, Style style) {
            return os << "\033[" << static_cast<int>(style) << "m";
        }

        static const Modifier _RESET_ALL(Style::RESET);
        static const Modifier _ERROR(Color::FG_RED);
        static const Modifier _INFO(Color::FG_GREEN);

        static inline std::string styled(std::string content, Style style) {
            std::stringstream ss;
            Modifier mod(style);

            ss << mod << content << _RESET_ALL;

            return ss.str();
        }

        static inline std::string mod(std::string content, Color fg, Color bg, Style style) {
            std::stringstream ss;
            Modifier mod(fg, bg, style);

            ss << mod << content << _RESET_ALL;

            return ss.str();
        }

        static inline std::string mod(std::string content, Color fg, Color bg = Color::BG_DEFAULT) {
            std::stringstream ss;
            Modifier mod(fg, bg);

            ss << mod << content << _RESET_ALL;

            return ss.str();
        }

        static inline std::string genMod(Color fg, Color bg = Color::BG_DEFAULT, Style style = Style::RESET) {
            std::stringstream ss;
            Modifier mod(fg, bg, style);

            ss << mod ;

            return ss.str();
        }

    }

    using ChatColor = Console::Color;
    using ChatStyle = Console::Style;

    inline std::ostream& applyColor(std::ostream& stream, ChatColor fg, ChatColor bg = ChatColor::BG_DEFAULT) {
        return stream << Console::Modifier(fg, bg);
    }

    inline std::ostream& resetStream(std::ostream& stream) {
        return stream << Console::_RESET_ALL;
    }

    inline Console::Color parseColor(char c, int colorOffset = 30) {
        int pick = c - 48; // 48 = ascii offset for '0'

        if (pick >= 0 && pick < 10 && pick != 8)
            return (Console::Color) (colorOffset + pick);
        
        return Console::Color::FG_DEFAULT;
    }

    // --- Transform JSON ---

    /*
    std::string epxandedJson(const std::string_view content) {
        std::stringstream buf;
        std::string spacer (0, '\t');

        int depth = 0;
        bool str {false};

        for (int i = 0; i < content.length(); i++) {
            if (content[i] == '{') {
                spacer = std::string(++depth * 3, ' ');

                buf << '{';
                buf << '\n';
                buf << spacer;
            } else if (content[i] == '}') {
                spacer = std::string(--depth * 3, ' ');

                buf << '\n';
                buf << spacer;
                buf << '}';
            } else if (content[i] == '[') {
                spacer = std::string(++depth * 3, ' ');

                buf << '[';
                buf << '\n';
                buf << spacer;
            } else if (content[i] == ']') {
                spacer = std::string(--depth * 3, ' ');

                buf << '\n';
                buf << spacer;
                buf << ']';
            } else if (content[i] == ',') {
                buf << ',';
                buf << '\n';
                buf << spacer;
            } else if (content[i] == ':') {
                buf << ':';
                buf << ' ';
            } else if (content[i] == '\'') {
                str = !str;

                buf << '\'';
            } else if (content[i] != ' ' || str) {
                buf << content[i];
            }
        }

        return buf.str();
    }
    */

    // ---v print styled format v---

    inline std::string sformat(const char * str) {
        std::stringstream buf;

        while (*str) {
            if (*str == '`') {
                if (*(str + 1) != 0)
                    buf << Console::Modifier(parseColor(*(str + 1), 40)); // 40 - background offset

                str += 2;
            } else if (*str == '$') {
                if (*(str + 1) == 0)
                    break;

                if (*(str + 1) == 'r')
                    buf << Console::_RESET_ALL;
                else if (*(str + 1) == 'b')
                    buf << Console::Modifier(Console::Style::BOLD);
                else if (*(str + 1) == 'i')
                    buf << Console::Modifier(Console::Style::ITALIC);
                else if (*(str + 1) == 'u')
                    buf << Console::Modifier(Console::Style::UNDERLINED);
                else if (*(str + 1) == 's')
                    (void) 0; // ignore %s without params
                else if (*(str + 1) == 'j') {
                    str += 2; // just skip this token, since $j can only be applied to left values (here = 0)
                } else
                    buf << Console::Modifier(parseColor(*(str + 1)));

                str += 2;
            } else
                buf << *(str++);
        }

        buf << Console::_RESET_ALL;

        return buf.str();
    }

    template <typename T, typename... Ts>
    std::string sformat(const char * str, const T& base, const Ts&... appendix) {
        std::stringstream buf;

        while (*str) {
            if (*str == '`') {
                if (*(str + 1) != 0)
                    buf << Console::Modifier(parseColor(*(str + 1), 40)); // 40 - background offset

                str += 2;
            } else if (*str == '$') {
                if (*(str + 1) == 0)
                    break;

                if (*(str + 1) == 'r')
                    buf << Console::_RESET_ALL;
                else if (*(str + 1) == 'b')
                    buf << Console::Modifier(Console::Style::BOLD);
                else if (*(str + 1) == 'i')
                    buf << Console::Modifier(Console::Style::ITALIC);
                else if (*(str + 1) == 'u')
                    buf << Console::Modifier(Console::Style::UNDERLINED);
                else
                    buf << Console::Modifier(parseColor(*(str + 1)));

                str += 2;
            } else if (*str == '%') {
                if (*(str + 1) == 0)
                    break;
                
                // ignore normal % symbols
                if (*(str + 1) != '%') {
                    buf << *(str++);
                    continue;
                }
                
                /* Format the type to a string using a user-defined realisation. */
                SFmt<T> fmt {};

                buf << fmt.toString(base);
                buf << sformat(str + 2, appendix...);

                return buf.str();
            } else
                buf << *(str++);
        }

        buf << Console::_RESET_ALL;

        return buf.str();
    }

    template <typename... Ts>
    void printsf(const char * str, const Ts&... appendix) {
        MS_IF_DEBUG {
            std::cout << sformat(str, appendix...) << std::endl;
        }
    }

    template <typename R, typename... Ts>
    R printsf(const R& ret, const char * str, const Ts&... appendix) {
        MS_IF_DEBUG {
            std::cout << sformat(str, appendix...) << std::endl;
        }

        return ret;
    }

    template <typename R, typename... Ts>
    R errorsf(const R& ret, const char * module, const char * str, const Ts&... appendix) {
        MS_IF_DEBUG {
            std::cout << sformat("$b$1<$3%%$1> ", module);
            std::cout << Console::Modifier(ChatColor::FG_RED, ChatStyle::BOLD);
            std::cout << sformat(str, appendix...) << std::endl;
        }

        return ret;
    }

    template <int level, typename... Ts>
    void printsf(const char * str, const Ts&... appendix) {
        if (ms_is_debug && (int) 10 >= level) {
            std::cout << sformat(str, appendix...) << std::endl;
        }
    }

    template <typename... Ts>
    void printsf_ignore_debug_mode(const char * str, const Ts&... appendix) {
        std::cout << sformat(str, appendix...) << std::endl;
    }

    //  (°)<  -swrrrt
    // (/_\)
    //  ^ ^

    // ---^ ................... ^---

    inline void resetAll() {
        std::cout << Console::_RESET_ALL << std::endl;
    }

    inline void print(const std::string& prefix, Console::Modifier& prefixMod, std::string& content, Console::Modifier& contentMod) {
        MS_IF_DEBUG {
            std::cout << prefixMod << prefix << Console::_RESET_ALL << contentMod << content << Console::_RESET_ALL << std::endl;
        }
    }

    inline void print(const std::string& str, const Console::Modifier& modifier) {
        MS_IF_DEBUG {
            std::cout << modifier << str << Console::_RESET_ALL << std::endl;
        }
    }

    inline void print(const std::string& str) {
        MS_IF_DEBUG {
            std::cout << str << std::endl;
        }
    }

    inline void error(const std::string& str) {
        MS_IF_DEBUG {
            print("Error: " + str, Console::BG_RED);
        }
    }

    /*
    void error(state code, const std::string& str) {
        MS_IF_DEBUG {
            print("[" + std::to_string(code) + "] Error: " + str, Console::BG_RED);
        }
    }
    */

    /*
    std::string stateCode(state s) {
        if (ms::debug_translate_state_codes)
            return /*util::toUpperCase(*/ /*getStateCode(s)*/ /*)*//*;
        
        return std::to_string(s);
    }
    */

    inline std::string stretch(const std::string& str, int len, char fill = ' ') {
        std::stringstream stream;

        stream << std::hex << str;

        if (str.length() < len) {
            for (int i = str.length(); i < len; i++)
                stream << fill;
        }

        return stream.str();;
    }

    template <typename T>
    std::string format(const char * x, const T& value) {
        std::stringstream buffer;

        while (*x) {
            if (*x == '%') {
                if (*(x + 1) == 0)
                    break;

                if (*(x + 1) == '%')
                    buffer << value;

                x++;
            } else
                buffer << *(x++);
        }

        return buffer.str();
    }

    template <typename T, typename... Ts>
    std::string format(const char * x, const T& value, const Ts&... values) {
        std::stringstream buffer;

        while (*x) {
            if (*x == '%') {
                if (*(x + 1) == 0)
                    break;

                if (*(x + 1) == '%')
                    buffer << value;
                
                buffer << format(x + 2, values...);

                return buffer.str();
            } else
                buffer << *(x++);
        }

        return buffer.str();
    }

    template <typename T>
    std::string toString(const T& elem) {
        return SFmt<T>{}.toString(elem);
    }

    /*
    std::string toString(InstructionSet& is, bool stopOnLIS = true) {
        std::stringstream str;
        uindex i = 0;
        uindex bytes = 0;

        Instruction err(Op::IIS);
        Instruction& ins = err;
        OpInfo info {"__ERROR__", Op::IIS, 0};

        str << "Index    Code   ID   Name          {0..4 parameters}" << std::endl;
        str << "----------------------------------------------------" << std::endl;

        for (i = 0; i < is.finalInstructions.size(); i++) {
            ins = is.finalInstructions[i];
            info = getOpInfo((Op) ins);
            bytes += sizeof(operation) + ins.paramCount() * sizeof(param);

            str << "0x" << std::hex << std::setfill('0') << std::setw(5) << i << "  ";
            str << "0x" << std::hex << std::setfill('0') << std::setw(3) << (unsigned int) info.b << "  ";
            
            if (ins.getLabel() != -1)
                str << std::dec << std::setfill('0') << std::setw(3) << ins.getLabel() << "  ";
            else if (i == is.baseOffset) // just a marker
                str << ">>>  ";
            else
                str << "     ";
            
            // instruction name
            str << stretch(info.a, 13);

            for (int j = 0; j < ins.paramCount(); j++)
                str << " 0x" << std::hex << std::setfill('0') << ins.getParam(j) << ((j < (ins.paramCount() - 1) || ins.hasVARG()) ? ',' : ' ');

            // variable string param
            if (ins.hasVARG())
                str << " '" << ins.getVARG() << '\'';

            str << std::endl;

            if (ins == Op::LIS && stopOnLIS)
                break;
        }

        str << '\n' << std::hex;
        str << "      Base offset: 0x" << is.baseOffset << '\n';

        str << std::dec;
        str << " Total operations: " << i << '\n';
        str << "       Bytes used: " << bytes << '\n';

        return str.str();
    }

    std::string toString(IData ** memory, usize size) {
        std::stringstream str;
        int nstart = -1, nend = -1; // null start, null end

        for (int i = 0; i < size; i++) {
            if (memory[i] == nullptr) {
                if (nstart == -1)
                    nstart = i;

                nend = i;
            } else {
                if (nstart != -1) {
                    str << "0x" << std::hex << std::setfill('0') << std::setw(5) << nstart << "...";
                    str << "0x" << std::hex << std::setfill('0') << std::setw(5) << nend << ": NIL" << std::endl;

                    nstart = nend = -1;
                }

                str << "0x" << std::hex << std::setfill('0') << std::setw(5) << i << ": " << memory::toString(memory[i], true) << std::endl;
            }
        }

        if (nstart != -1) {
            str << "0x" << std::hex << std::setfill('0') << std::setw(5) << nstart << "...";
            str << "0x" << std::hex << std::setfill('0') << std::setw(5) << nend << ": NIL" << std::endl;
        }

        return str.str();
    }
    */

    // -- Smart Debug

    static int debug_level = 0;

    inline void beginSection(const std::string& content) {
        MS_IF_DEBUG {
            for (int i = 0; i < debug_level; i++)
                std::cout << "| ";

            std::cout << " / -- " << sformat(content.c_str()) << std::endl;
            debug_level++;
        }
    }

    inline void sdebug(const std::string& content) {
        MS_IF_DEBUG {
            for (int i = 0; i < debug_level; i++)
                std::cout << "| ";

            std::cout << sformat(content.c_str()) << std::endl;
        }
    }

    template <typename... Ts>
    inline void sdebug(const std::string& content, const Ts&... args) {
        MS_IF_DEBUG {
            for (int i = 0; i < debug_level; i++)
                std::cout << "| ";

            std::cout << sformat(content.c_str(), args...) << std::endl;
        }
    }

    /*
    state endSection(const std::string& content = "") {
        MS_IF_DEBUG {
            debug_level--;

            for (int i = 0; i < debug_level; i++)
                std::cout << "| ";

            std::cout << " \\ ------ " << sformat(content.c_str()) << std::endl;
        }

        return 0;
    }
    */

}
    

#endif
