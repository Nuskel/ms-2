#include "source.hpp"
#include "misc.hpp"
#include "debug.hpp"
#include "sformats.hpp"

namespace ms {

  // -- Vocabulary
  MS_VALUE_MAP(std::string, Tok) tokenMap {
    // key words
    { "module",     Tok::KW_MODULE 		},
    { "import",     Tok::KW_IMPORT    },
    { "end",        Tok::KW_END       },

    { "def",        Tok::KW_DEF 			},
    { "return",     Tok::KW_RETURN    },
    { "if",         Tok::KW_IF 				},
    { "else",       Tok::KW_ELSE 			},
    { "let",        Tok::KW_LET       },

    { "internal",   Tok::KW_INTERNAL  },

    // operators
    { "=",          Tok::OP_ASSIGN    },

    { ".",          Tok::OP_DOT				},
    { ",",          Tok::OP_COMMA     },
    { ":",          Tok::OP_COLON     },
    { "...",        Tok::OP_ELLIPSIS 	},

		{ "+",					Tok::OP_ADD 			},
		{ "-",					Tok::OP_SUB 			},
		{ "*",					Tok::OP_MUL 			},
		{ "/",					Tok::OP_DIV 			},
		{ "%",					Tok::OP_MOD				},

    { "(",					Tok::OP_LEFT_PARENTHESIS },
    { ")",          Tok::OP_RIGHT_PARENTHESIS },
    { "[",          Tok::OP_LEFT_BRACKET },
    { "]",          Tok::OP_RIGHT_BRACKET },
    { "{",          Tok::OP_LEFT_CURLY },
    { "}",          Tok::OP_RIGHT_CURLY },
  };
  MS_MAP_LOOKUP(strToTok, tokenMap, std::string, Tok, Tok::UNKNOWN);

  // CODE READING

  Status readCode(Source& source, const std::string& code) {
    return Status::FAIL;
  }

  Status readFile(Source& source, const std::string& filename) {
    if (source.state != SourceState::UNREAD) {
      return Status::FAIL;
    }

    std::string line;
    std::ifstream in(filename);
    std::stringstream build;

    debug::printsf("[Source] Reading file %%...", filename);

    if (in.is_open()) {
      while (std::getline(in, line)) {
        build << line << static_cast<char>('\n');
        source.lines++;
      }

      in.close();
    } else {
      debug::printsf("[Source] File cannot be read (does it exist?).");

      return Status::FAIL;
    }

    source.code = build.str();
    source.state = SourceState::READ;

    debug::printsf("[Source] File read successfully.");
    debug::printsf("%%", source.code);

    return Status::SUCCESS;
  }

  // LEXING

  Status lex(Source& source) {
    if (source.state != SourceState::READ) {
      return Status::FAIL;
    }

    const size_t len { source.code.length() };

    // -- State

    Status status { Status::SUCCESS };
    Lexer lexer { source };

    bool str { false };
    bool comment { false };
    bool multilineComment { false };

    // --

    for (size_t i = 0; i < len; i++) {
      const char c = source.code.at(i);

      if (comment) {
        if (c != 10) // new line
          continue;

        comment = false;
      }

      if (multilineComment) {
        // -- multi line comment end
        if (c == '*' && i + 2 < len && source.code[i + 1] == '-' && source.code[i + 2] == '-') {
          multilineComment = false;

          i += 2;
          lexer.pos += 2;
        } else if (c == 10 /* new line */) {
          lexer.col = 0;
          lexer.scol = 0;
          lexer.line++;
        }

        continue;
      }

      // appending to string
      if (str) {
        if (c == '\'') {
          str = false;

          // commit whole string as a single token
          lexer.commit(Tok::L_STRING);
        } else {
          lexer.append(c);
        }

        // TODO: break on \n?

        continue;
      }

      switch (c) {
        case 10 /* new line */:
          lexer.commit();
          lexer.col = 0;
          lexer.scol = 0;
          lexer.line++;

          break;

        // -- flush last token
        case ' ':
          lexer.commit();
          lexer.col += 1;

          break;

        // -- strings
        case '\'':
          str = true;

          break;

        // -- tokens starting with '-'
        case '-': {

          // -- line comment
          if (i + 1 < len & source.code[i + 1] == '-') {

            // -- multi line comment
            if (i + 2 < len && source.code[i + 2] == '*') {
              multilineComment = true;

              i += 2;
              lexer.pos += 2;

              break;
            }

            comment = true;

            break;
          }

          lexer.commit();
          lexer.push(Tok::OP_SUB, c);

          break;
        }

        // -- tokens starting with '.'
        case '.':

          // -- ellipsis
          if (i + 2 < len && source.code[i + 1] == '.' && source.code[i + 2] == '.') {
            lexer.push(Tok::OP_ELLIPSIS);

            i += 2;
            lexer.pos += 2;

            break;
          }

          // -- number
          if (i + 1 < len && isDigit(source.code[i + 1])) {
            break;
          }

          // -- function or attribute splitter
          lexer.commit();
          lexer.push(Tok::OP_DOT, c);

          break;

        default:
          // -- standard single-character tokens
          Tok t = strToTok(std::string(1, c));

          if (t != Tok::UNKNOWN) {
            lexer.commit();
            lexer.push(t, c);
          } else {
            // others characters are added to the current token
            lexer.append(c);
          }

          break;
      }

      lexer.scol++;
      //lexer.col++;
      lexer.pos++;
    }

    // push last active token
    lexer.commit();

    // transfer lexer state to source
    source.lines = lexer.line + 1;

    // use status from the lexer
    status = lexer.status;

    if (status == Status::SUCCESS)
      source.state = SourceState::LEXED;

    debug::printsf("[Lex] $3%%$r | $3%%$r Lines", lexer.status, source.lines);

    return status;
  }

  void Lexer::commit(Tok tok) {
    if (buflen > 0) {
      std::string content = buf.str();

      if (tok == Tok::UNKNOWN)
        tok = strToTok(content);

      if (tok == Tok::UNKNOWN) {
        if (isNaturalNumber(content)) {
          Token t;

          t.value = content;
          t.type = Tok::L_INTEGRAL;
          t.line = line;
          t.col = col;
          t.tcol = tcol;
          t.integral = parseInt(content);

          col += t.value.length();

          source.tokens.push_back(t);
        } else
          push(Tok::IDENTIFIER);
      } else
        push(tok);
    }

    // start column of next token is next token
    // scol = col + 1;

    resetBuf();
  }

  void Lexer::push(Tok tok) {
    Token t;

    t.value = buf.str();
    t.type = tok;
    t.line = line;
    t.col = col;
    t.tcol = tcol;

    col += t.value.length();

    source.tokens.push_back(t);
  }

  void Lexer::push(Tok tok, char c) {
    Token t;

    t.value = std::string(1, c);
    t.type = tok;
    t.line = line;
    t.col = col;
    t.tcol = tcol;

    col += t.value.length();

    source.tokens.push_back(t);
  }

  void Lexer::push(char c) {
    Token t;

    t.value = std::to_string(c);
    t.type = strToTok(t.value);
    t.line = line;
    t.col = col;
    t.tcol = tcol;

    col += t.value.length();

    source.tokens.push_back(t);
  }

  // SOURCE

  std::string Source::getLine(size_t line) const {
    if (line >= lines)
      return "";
    
    size_t begin {0}, i {0}, l {0};

    for (; i < code.length(); i++) {
      if (code.at(i) == 10) {
        if (l == line)
          break;
        
        begin = ((i + 1) < code.length() ? (i + 1) : i);
        l++;
      }
    }

    return code.substr(begin, i - begin);
  }

  std::string Source::getMarkedLine(uint line, uint from /* actual string pos, NOT token pos! */, uint to) {
    std::stringstream buf;
    std::string lstr { getLine(line) };
    int offset = 0; // length of (file|line:column)
    
    // offset += std::log10(line + 1) + 1;
    //offset += std::log10(from + 1) + 1;

    buf << debug::Console::Modifier(debug::ChatColor::FG_RED);
    offset -= buf.str().length(); // ignore styling
    
    if (from == to) {
      buf << "(" << file << "|" << (line + 1) << ":" << (from + 1) << ") ";
      offset += buf.str().length();
    } else {
      buf << "(" << file << "|" << (line + 1) << ":" << (from + 1) << '-' << (to + 1) << ") ";
      offset += buf.str().length();
    }

    buf << debug::Console::Modifier(debug::ChatColor::FG_WHITE);
    buf << lstr;
    
    if (to < from)
      buf << "...";
    
    buf << '\n' << std::string(offset + from, ' ');
    buf << debug::Console::Modifier(debug::ChatColor::FG_RED);

    if (from == to) {
      buf << "^";
    } else if (to > from) {
      //buf << "^" << std::string(to - from, '~');
      buf << from << " .. " << to;
    }

    debug::resetStream(buf);

    return buf.str();
  }

  std::ostream& Source::write(std::ostream& stream, const SourceLocation& location, bool marker) const {
    /* we need the token for line and column; else nothing to do here */
    if (location.start == -1 || location.start >= tokenCount())
      return stream;
    
    const size_t end = location.end - 1; /* get inclusive end token */
    const Token& from = tokens[location.start];
    const size_t line = from.line + 1;
    const size_t colFrom = from.col + 1;

    size_t offset {0};
    size_t range {1};

    stream << debug::Console::Modifier(debug::ChatColor::FG_RED);

    /* Just print the position and simple marker when:
     *  a) end is set to -1 (not present)
     *  b) end is invalid (>= max tokens || <= start)
     */
    if (location.end == -1 || end >= tokenCount() || end <= location.start) {
      stream << '(' << file << '|' << line << ':' << colFrom << ") ";
      
      offset += 5;
      offset += file.length();
      offset += std::log10(line) + 1;
      offset += std::log10(colFrom) + 1;
    } else {
      const Token& to = tokens[end];
      const size_t colTo = to.col + 1;

      stream << '(' << file << '|' << line << ':' << colFrom << '-' << colTo << ") ";
      
      offset += 6;
      offset += file.length();
      offset += std::log10(line) + 1;
      offset += std::log10(colFrom) + 1;
      offset += std::log10(colTo) + 1;

      range = colTo - colFrom;
    }

    stream << debug::Console::Modifier(debug::ChatColor::FG_WHITE);
    stream << getLine(from.line);

    if (marker) {
      offset += from.col;

      stream << '\n';
      stream << std::string(offset, ' ');
      stream << debug::Console::Modifier(debug::ChatColor::FG_RED);
      stream << '^';

      if (range == 1)
        stream << std::string(from.value.length() - 1 /* 1 for ^ */, '~');
      if (range > 1 && range < 99 /* max length */)
        stream << std::string(range, '~');
    }

    stream << '\n';

    debug::resetStream(stream);

    return stream;
  }

  std::string Source::concat(uint from, uint to) {
    std::stringstream buf;

    for (uint i = from; i < to; i++) {
      buf << tokens[i].value;

      if (i < (to - 1))
        buf << ' ';
    }

    return buf.str();
  }

  Status Source::readIdentifier(std::string& out, long position) {
    size_t access = token + 1;

    if (position != -1)
      access = static_cast<size_t>(position);

    if (!validIndex(access))
      return Status::FAIL; // ERR_

    if (tokens[access].type != Tok::IDENTIFIER)
      return Status::FAIL; // ERR_INVALID_TOKEN_TYPE

    out = tokens[access].value;

    // update counter if not a special position was given
    if (position == -1)
      token = access;

    return Status::SUCCESS;
  }

  // TOKEN META

  TokenClass classifyToken(const Tok token) {
    if (token == Tok::UNKNOWN)
      return TokenClass::UNKNOWN;

    if (token == Tok::IDENTIFIER)
      return TokenClass::IDENTIFIER;

    if (token >= Tok::KW_IMPORT && token <= Tok::KW_ELSE)
      return TokenClass::KEYWORD;

    if (token >= Tok::OP_ASSIGN && token <= Tok::OP_RIGHT_CURLY)
      return TokenClass::OPERATOR;

    if (token >= Tok::L_INTEGRAL && token <= Tok::L_STRING)
      return TokenClass::LITERAL;

    return TokenClass::UNKNOWN;
  }

  bool isParenthesis(const Tok token) {
    return token >= Tok::OP_LEFT_PARENTHESIS &&
      token <= Tok::OP_RIGHT_CURLY;
  }

}