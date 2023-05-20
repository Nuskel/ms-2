#ifndef MS_SOURCE_HPP
#define MS_SOURCE_HPP

#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
#include <fstream>
#include <sstream>
#include <utility>
#include <cmath>

#include "env.hpp"
#include "debug.hpp"

namespace ms {

  enum class Tok {

    UNKNOWN,
    IDENTIFIER,

    /* import <module>
     *  @module - equals the file name without extension in any source location
     *
     * Will lookup the given file and parse its content before continuing.
     */
    KW_IMPORT,
    KW_MODULE,
    KW_DEF,

    KW_LET,

    KW_IF,
    KW_ELSE,

    //
    OP_ASSIGN,

    // Control Operators
    OP_DOT,
    OP_ELLIPSIS,

    // Arithmetic Operators
		OP_ADD,
		OP_SUB,
		OP_MUL,
		OP_DIV,
		OP_MOD,

		OP_LEFT_PARENTHESIS,
		OP_RIGHT_PARENTHESIS,

    // Literals
    L_INTEGRAL,
    L_DECIMAL,
    L_STRING

  };

  enum class TokenClass {

    UNKNOWN,
    IDENTIFIER,

    KEYWORD,
    OPERATOR,
    LITERAL

  };

  struct Token {

    Tok type;
    std::string value;
    ulong line {0};
    ulong col {0};
    ulong tcol {0};

    union {
      long long integral = 0;
      long double decimal;
    };

  };

  enum class SourceState {

    UNREAD,
    READ,
    LEXED,
    PARSED,
    COMPILED,
    LINKED

  };

  struct Source {

    SourceState state { SourceState::UNREAD };
    std::string name;
    std::string file;
    std::string code;
    std::vector<Token> tokens;
    size_t lines { 0 };

    // Scan State
    size_t token { 0 };
    size_t line { 0 };

		Source() = default;

    /* Read the next token and return the value if it is a Tok::IDENTIFIER.
     * If position is set (position <> -1), the token at this index will be
     * read. Otherwise the current token (at index {token}) will be read and
     * the token-counter will be increased.
     */
    Status readIdentifier(std::string&, long position = -1);

    std::string getLine(uint line);

    std::string getMarkedLine(uint line, uint from, uint to);

    std::string concat(uint from, uint to);

    inline bool validIndex(long index) {
      return index >= 0 && index < tokenCount();
    }

    inline bool hasMore(size_t amount = 1) {
      return (token + amount) < tokenCount();
    }

    inline size_t tokenCount() {
      return tokens.size();
    }

  };

  struct CodePos {

    uint line;
    uint column;

    CodePos(uint p_line = 0, uint p_col = 0) : line(p_line), column(p_col) {}

  };

  struct Lexer {

    Source& source;
    Status status { Status::SUCCESS };

    ulong line {0};
    ulong col {0}, tcol {0}, scol {0}; // character column; token column (token index in line); start column of current buffer
    ulong pos {0};

    std::stringstream buf;
    ulong buflen { 0 };

    bool multi { false };

    // --

    Lexer(Source& src) : source(src) {}

    // --

    void commit(Tok tok = Tok::UNKNOWN);

    void push(Tok);
    
    void push(char);

    inline void append(char c) {
      buf << c;
      buflen++;
    }

    inline void resetBuf() {
      buf.str("");
      buflen = 0;
    }

  };

	using SourceMap = std::unordered_map<std::string, SRef<Source>>;

  /****************************************
   *
   ****************************************/

  Status readCode(Source& target, const std::string& code);
  Status readFile(Source& target, const std::string& filename);

  Status lex(Source& source);

  TokenClass classifyToken(const Tok token);

}

#endif
