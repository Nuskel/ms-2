#ifndef MS_SFORMATS_HPP
#define MS_SFORMATS_HPP

#include "debug.hpp"
#include "env.hpp"
#include "source.hpp"
#include "lang.hpp"
#include "context.hpp"
#include "ast.hpp"
#include "types.hpp"

namespace ms {
    
    template <>
    struct SFmt<Status> {

        std::string toString(const Status s) {
            switch (s) {
                case Status::SUCCESS: return "SUCCESS";
                case Status::FAIL: return "FAIL";
                case Status::INTERNAL: return "INTERNAL";

                default:
                return std::string("(Status) ") + std::to_string((int) s);
            }
        }

    };

    template <>
    struct SFmt<Tok> {

        std::string toString(const Tok tok) {
            switch (tok) {
                case Tok::UNKNOWN: return "UNKNOWN";
                case Tok::IDENTIFIER: return "IDENTIFIER";

                case Tok::KW_MODULE: return "KW_MODULE";
                case Tok::KW_IMPORT: return "KW_IMPORT";
                case Tok::KW_DEF: return "KW_DEF";
                case Tok::KW_RETURN: return "KW_RETURN";

                case Tok::KW_LET: return "KW_LET";

                case Tok::KW_IF: return "KW_IF";
                case Tok::KW_ELSE: return "KW_ELSE";

                case Tok::OP_ASSIGN: return "OP_ASSIGN";

                case Tok::OP_DOT: return "OP_DOT";
                case Tok::OP_COMMA: return "OP_COMMA";
                case Tok::OP_ELLIPSIS: return "OP_ELLIPSIS";

                case Tok::OP_ADD: return "OP_ADD";
                case Tok::OP_SUB: return "OP_SUB";
                case Tok::OP_MUL: return "OP_MUL";
                case Tok::OP_DIV: return "OP_DIV";

                case Tok::OP_LEFT_PARENTHESIS: return "OP_LEFT_PARENTHESIS";
                case Tok::OP_RIGHT_PARENTHESIS: return "OP_RIGHT_PARENTHESIS";

                case Tok::L_INTEGRAL: return "L_INTEGRAL";
                case Tok::L_DECIMAL: return "L_DECIMAL";
                case Tok::L_STRING: return "L_STRING";

                default:
                return std::string("(Token) ") + std::to_string((int) tok);
            }
        }
    };

    template <>
    struct SFmt<ast::NodeType> {
        std::string toString(ast::NodeType type) {
            switch (type) {
                case ast::NodeType::None: return "None";
                case ast::NodeType::Program: return "Program";
                case ast::NodeType::Module: return "Module";
                case ast::NodeType::ImportStmt: return "ImportStmt";
                case ast::NodeType::AssignStmt: return "AssignStmt";
                case ast::NodeType::TypeDecl: return "TypeDecl";
                case ast::NodeType::TypeRef: return "TypeRef";
                case ast::NodeType::FnDecl: return "FnDecl";
                case ast::NodeType::VarDecl: return "VarDecl";
                case ast::NodeType::LiteralExpr: return "LiteralExpr";
                case ast::NodeType::RefExpr: return "RefExpr";
                case ast::NodeType::BinaryExpr: return "BinaryExpr";
                case ast::NodeType::CommaExpr: return "CommaExpr";
                case ast::NodeType::FnCall: return "FnCall";
                case ast::NodeType::ProgramBlock: return "ProgramBlock";
                case ast::NodeType::ParamsBlock: return "ParamsBlock";
                case ast::NodeType::FnBodyBlock: return "FnBodyBlock";

                default:
                    return std::string("(NodeType) ") + std::to_string((int) type);
            }
        }
    };

    template <>
    struct SFmt<EntityType> {

        std::string toString(EntityType type) {
            switch (type) {
                case EntityType::VAR: return "VAR";
                case EntityType::OBJECT: return "OBJECT";
                case EntityType::PROTO: return "PROTO";
                case EntityType::NAMESPACE: return "NAMESPACE";
                case EntityType::MODULE: return "MODULE";
                case EntityType::UNKNOWN: return "UNKNOWN";

                default:
                    return std::string("(EntityType) ") + std::to_string((int) type);
            }
        }

    };

    template <>
    struct SFmt<Literal> {

        std::string toString(const Literal& lit) {
            if (lit.type == types::String) {
                return "@" + std::to_string(lit.id);
            } else if (lit.type == types::Int) {
                return std::to_string(std::get<msx::Integral>(lit.value));
            } else if (lit.type == types::Decimal) {
                return std::to_string(std::get<msx::Decimal>(lit.value));
            }
        
            return "unknown literal";
        }

        std::string toVerboseString(const Literal& lit) {
            if (lit.type == types::String) {
                return "'" + std::get<std::string>(lit.value) + "'";
            } else if (lit.type == types::Int) {
                return std::to_string(std::get<msx::Integral>(lit.value)) + "i";
            } else if (lit.type == types::Decimal) {
                return std::to_string(std::get<msx::Decimal>(lit.value)) + "d";
            }
        
            return "unknown literal";
        }

    };

    template <>
    struct SFmt<Type> {

        std::string toString(Type type) {
            switch (type.typeClass()) {
                case TypeClass::SIMPLE:
                    return type.name().value;

                case TypeClass::REFERENCE:
                    return type.name().value + "&";

                case TypeClass::OBJECT:
                    return type.name().value + "{}";

                case TypeClass::ARRAY:
                    return type.name().value + "<" + SFmt<Type>{}.toString(derive<TypeDef, Array>(type.def)->arrayType) + ">[]";

                case TypeClass::TUPEL:
                    return type.name().value + "()";

                default:
                    return type.name().value + "?";
            }
        }

    };
    
    template <>
    struct SFmt<Value> {

        std::string toString(Value value) {
            switch (value.valueClass) {
                case ValueClass::LITERAL:
                    return SFmt<Literal>{}.toString(std::get<Literal>(value.content));

                case ValueClass::REFERENCE:
                    return std::to_string(value.asAddress()) + "_r";

                default:
                    return "Value<?>";
            }
        }

    };

    template <>
    struct SFmt<TypedValue> {

        std::string toString(const TypedValue& tv) {
            return debug::format("{%%, %%}", SFmt<Value>{}.toString(tv.value), SFmt<Type>{}.toString(tv.type));
        }

    };

    template <>
    struct SFmt<types::Operator> {

        std::string toString(types::Operator op) {
            switch (op) {
                case types::Operator::Add: return "+";
                case types::Operator::Sub: return "-";
                case types::Operator::Mul: return "*";
                case types::Operator::Div: return "/";
                case types::Operator::Comma: return ",";

                default:
                    return std::string("(Operator) ") + std::to_string((int) op);
            }
        }

    };

    template <>
    struct SFmt<Instruction> {

        std::string toString(Instruction i) {
            return std::to_string((int) i.op);
        }

    };

    template <>
    struct SFmt<OpArg> {

        std::string toString(const OpArg& arg) {
            switch (arg.type) {
                case OpArg::Type::NONE:
                    return "<none>";

                case OpArg::Type::INTERMEDIATE_NUMERIC:
                    return std::to_string((size_t) arg.intermediate.numeric);

                default:
                    return "Arg<?>";
            }
        }

    };

    template <>
    struct SFmt<memloc> {

        std::string toString(const memloc& ml) {
            switch (ml.type) {
                case memloc::INTERMEDIATE:
                    return "#" + std::to_string(ml.intermediate.integral);

                case memloc::ADDR_LOCAL:
                    return "l" + std::to_string(ml.offset);
                
                default:
                    return "memloc(" + std::to_string(ml.type) + ")";
            }
        }

    };

    /* -------- */

    static inline void printSources(Context& ctx) {
        debug::printsf("Source Locations (%%):", ctx.sourceLocations.size());

        for (const auto& location : ctx.sourceLocations) {
            debug::printsf(" * %%", location);
        }
        
        debug::printsf("");
        debug::printsf("Sources (%%): (> for entry source)", ctx.sources.size());

        for (const auto& source : ctx.sources) {
            debug::printsf(" %% %% \t (%%) | %% lines | %% tokens",
                source.second == ctx.entrySource ? ">" : "*",
                source.second->name,
                source.first,
                source.second->lines,
                source.second->tokens.size()
            );
        }
    }

    static inline void printInstructions(const Instructions& is) {
        std::stringstream str;
        size_t i = 0;
        size_t bytes = 0;

        str << " Index    Code   ID   Name          {0..4 parameters}" << std::endl;
        str << "-----------------------------------------------------" << std::endl;

        Instruction none = { Op::NOP };
        Instruction& ins = none;
        OpInfo info;

        for (i = 0; i < is.instructions.size(); i++) {
            ins = is.instructions[i];
            info = getOpInfo(ins.op);
            // bytes += sizeof(operation) + ins.paramCount() * sizeof(param);

            if (ins.label.length() > 0) {
                str << ins.label << ":\n";
            }

            str << " "; // left margin
            str << "0x" << std::hex << std::setfill('0') << std::setw(5) << i << "  ";
            str << "0x" << std::hex << std::setfill('0') << std::setw(3) << (unsigned int) info.operation << "  ";
            
            if (i == is.baseOffset) // just a marker
                str << ">>>  ";
            else
                str << "     ";
            
            // instruction name
            // str << stretch(info.a, 13);
            str << info.name;

            // TODO: loop: see below
            if (ins.argCount == 0) {
                str << ' ' << SFmt<memloc>{}.toString(ins.target) << " " << SFmt<memloc>{}.toString(ins.target);
            }

            /*
            for (int j = 0; j < ins.paramCount(); j++)
                str << " 0x" << std::hex << std::setfill('0') << ins.getParam(j) << ((j < (ins.paramCount() - 1) || ins.hasVARG()) ? ',' : ' ');

            // variable string param
            if (ins.hasVARG())
                str << " '" << ins.getVARG() << '\'';
            */

            str << '\n';

            //if (ins == Op::LIS && stopOnLIS)
            //    break;
        }

        str << '\n' << std::hex;
        str << "      Base offset: 0x" << is.baseOffset << '\n';

        str << std::dec;
        str << " Total operations: " << i << '\n';
        str << "       Bytes used: " << bytes << '\n';

        std::cout << str.str() << '\n';
    }

    static inline std::string structureString(SRef<Namespace> block, int depth = 0) {
        std::stringstream buffer;

        std::string space {std::string(depth, ' ')};
        int index = 0;

        if (depth >= 0)
            buffer << block->symbol << " (" << debug::toString(block->type) << ") " << "level" << "-" << "address" << "\n";

        for (const auto& e : block->entities) {
            buffer << space;
            buffer << "|-";

            if (!e.second.entity) {
                buffer << e.first << " (decl)\n";
            } else if (e.second.entity->isNamespace()) {
                buffer << structureString(std::dynamic_pointer_cast<Namespace>(e.second.entity), depth + 1);
            } else
                buffer << e.first << " (" << debug::toString(e.second.entity->type) << "), " << e.second.entity << "%" << "e.value->localAddress" << "\n";
        }

        return buffer.str();
    }

}

#endif