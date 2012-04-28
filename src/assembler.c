/*
 * assembler.c
 * By Steven Smith
 */

#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum token {
    token_invalid, /* Flag for an invalid token */
    token_ident, /* Generic identifier, gets replaced later */
    token_number,
    token_label,
    /* Punctuation */
    token_comma,
    token_colon,
    token_leftbracket,
    token_rightbracket,
    token_plus,
    /* Registers */
    token_reg_a,
    token_reg_b,
    token_reg_c,
    token_reg_x,
    token_reg_y,
    token_reg_z,
    token_reg_i,
    token_reg_j,
    /* Register-like keywords */
    token_pc,
    token_sp,
    token_ex,
    token_pop,
    token_peek,
    token_push,
    token_pick,
    /* Basic opcodes */
    token_set,
    token_add,
    token_sub,
    token_mul,
    token_mli,
    token_div,
    token_dvi,
    token_mod,
    token_mdi,
    token_and,
    token_bor,
    token_xor,
    token_shr,
    token_asr,
    token_shl,
    token_ifb,
    token_ifc,
    token_ife,
    token_ifn,
    token_ifg,
    token_ifa,
    token_ifl,
    token_ifu,
    token_adx,
    token_sbx,
    token_sti,
    token_std,
    /* Non-basic opcodes */
    token_jsr,
    token_hcf,
    token_int,
    token_iag,
    token_ias,
    token_rfi,
    token_iaq,
    token_hwn,
    token_hwq,
    token_hwi
};

typedef struct token_list {
    enum token type;
    char *ident;
    int   value;
    int   line_number;
    int   col_number;
    struct token_list *next;
} token_list;

#define IDENTBUF_SIZE 80
static char ident_buf[IDENTBUF_SIZE + 1];

#define CHARBUF_SIZE 256
static char charbuf[CHARBUF_SIZE];
/* Default value forces file read on first access */
static int charbuf_pos = CHARBUF_SIZE;
static int charbuf_end = CHARBUF_SIZE;

/* Buffered char read. Also allows us to filter input to simplify parsing. */
static int get_next_char( FILE *fp ) {
    int i;
    if( charbuf_pos >= CHARBUF_SIZE ) {
        /* Out of buffer space, time to refill it */
        i = fread(charbuf, sizeof(char), CHARBUF_SIZE, fp);
        if( i != CHARBUF_SIZE ) {
            /* Hit an error/EOF */
            charbuf_end = i;
        }
        charbuf_pos = 0;
    }
    if( charbuf_pos < charbuf_end ) {
        i = charbuf[charbuf_pos++];
        /* Tab to space */
        if( i == '\t' ) {
            return (int)' ';
        }
        /* Munch carrier feed */
        else if( i == '\r' ) {
            return get_next_char(fp);
        }
        /* If it's alpha, then force lower case */
        else {
            return tolower((int)i);
        }
    }
    else {
        return EOF;
    }
}

static token_list * build_token( enum token type, int line, int col ) {
    token_list *result;
    result = (token_list*)malloc(sizeof(token_list));
    result->type        = type;
    result->ident       = NULL;
    result->value       =    0;
    result->line_number = line;
    result->col_number  =  col;
    result->next        = NULL;
    return result;
}

/* This is just to simplify the list building */
static token_list * build_invalid_token() {
    return build_token(token_invalid, 0, 0);
}

static token_list * build_token_ident( int pos, int line, int col ) {
    char *s;
    int i;
    token_list *result;
    s = (char*)malloc(sizeof(char) * pos + 1);
    /* Copy string from ident buffer */
    for( i = 0; i < pos; i++ ) {
        s[i] = ident_buf[i];
    }
    s[pos] = '\0';
    result = build_token(token_ident, line, col);
    result->ident = s;
    return result;
}

static token_list * build_token_hex( int pos, int line, int col ) {
    token_list *result;
    result = build_token(token_number, line, col);
    ident_buf[pos] = '\0';
    result->value = strtol(ident_buf, NULL, 16);
    return result;
}

static token_list * build_token_dec( int pos, int line, int col ) {
    token_list *result;
    result = build_token(token_number, line, col);
    ident_buf[pos] = '\0';
    result->value = strtol(ident_buf, NULL, 10);
    return result;
}

/* Currently unused
static token_list * build_token_bin( int pos, int line, int col ) {
    token_list *result;
    result = build_token(token_number, line, col);
    ident_buf[pos] = '\0';
    result->value = strtol(ident_buf, NULL, 2);
    return result;
}
*/

static void free_token_list( token_list *tok ) {
    token_list *next;
    while( tok ) {
        next = tok->next;
        if( tok->ident ) {
            free(tok->ident);
        }
        free(tok);
        tok = next;
    }
}

/* First phase of the lexer simply separates lexemes into punctuation, numbers
   (in their various forms), and general identifiers. The identifiers will be
   tokenized in the second phase. */

static token_list * build_token_list_f( FILE *fp ) {
    int curr_char;
    int ident_pos;
    int current_line;
    int current_col;
    token_list *result;
    token_list *result_end;
    if( !fp ) {
        fprintf(stderr, "ERROR: Bad file handle\n");
        return NULL;
    }
    result = result_end = build_invalid_token();
    curr_char = get_next_char(fp);
    current_line = current_col = 1;
    while( curr_char != EOF ) {
        /* Ident builder */
        if( isalpha(curr_char) ) {
            ident_pos = 0;
            do {
                ident_buf[ident_pos] = curr_char;
                ident_pos++;
                curr_char = get_next_char(fp);
            } while( curr_char != EOF && isalnum(curr_char) && ident_pos < IDENTBUF_SIZE );
            result_end->next = build_token_ident(ident_pos, current_line, current_col);
            result_end = result_end->next;
            current_col += ident_pos;
            continue;
        }
        /* Number builder */
        else if( isnumber(curr_char) ) {
            /* Need to grab the second char to see if we're doing decimal, hex, or binary */
            ident_buf[0] = curr_char;
            ident_pos = 1;
            curr_char = get_next_char(fp);
            /* Hex */
            if( curr_char == 'x' ) {
                ident_pos = 0;
                curr_char = get_next_char(fp);
                if( !ishexnumber(curr_char) ) {
                    fprintf(stderr, "ERROR: Expected hexadecimal number (Line: %d, Col: %d)\n",
                            current_line, current_col);
                    free_token_list(result);
                    return NULL;
                }
                else {
                    while( curr_char != EOF && ishexnumber(curr_char) && ident_pos < IDENTBUF_SIZE ) {
                        ident_buf[ident_pos] = curr_char;
                        ident_pos++;
                        curr_char = get_next_char(fp);
                    }
                }
                result_end->next = build_token_hex(ident_pos, current_line, current_col);
                result_end = result_end->next;
            }
            /* TODO: Binary */
            /* Decimal */
            else {
                while( curr_char != EOF && isnumber(curr_char) && ident_pos < IDENTBUF_SIZE ) {
                    ident_buf[ident_pos] = curr_char;
                    ident_pos++;
                    curr_char = get_next_char(fp);
                }
                result_end->next = build_token_dec(ident_pos, current_line, current_col);
                result_end = result_end->next;
            }
            current_col += ident_pos;
            continue;
        }
        /* Commas */
        else if( curr_char == ',' ) {
            result_end->next = build_token(token_comma, current_line, current_col);
            result_end = result_end->next;
        }
        /* Colons */
        else if( curr_char == ':' ) {
            result_end->next = build_token(token_colon, current_line, current_col);
            result_end = result_end->next;
        }
        /* Left bracket */
        else if( curr_char == '[' ) {
            result_end->next = build_token(token_leftbracket, current_line, current_col);
            result_end = result_end->next;
        }
        /* Right bracket */
        else if( curr_char == ']' ) {
            result_end->next = build_token(token_rightbracket, current_line, current_col);
            result_end = result_end->next;
        }
        /* Plus */
        else if( curr_char == '+' ) {
            result_end->next = build_token(token_plus, current_line, current_col);
            result_end = result_end->next;
        }
        /* White space */
        else if( curr_char == ' ' ) {
            /* Do nothing */
        }
        /* Newlines */
        else if( curr_char == '\n' ) {
            current_line++;
            current_col = 0; /* This gets incremented immediately */
        }
        /* Comments */
        else if( curr_char == ';' ) {
            /* Eat everything until EOL/EOF */
            do {
                curr_char = get_next_char(fp);
            } while( curr_char != EOF && curr_char != '\n' );
            continue;
        }
        /* Don't know how to handle whatever this is */
        else {
            fprintf(stderr, "ERROR: Lexer failed on input '%c' (Line: %d, Col: %d)\n",
                    curr_char, current_line, current_col);
            free_token_list(result);
            return NULL;
        }
        curr_char = get_next_char(fp);
        current_col++;
    }
    /* Need to axe the head of the list. */
    if( result->next ) {
        result_end = result->next;
        free(result);
        return result_end;
    }
    /* No valid tokens received */
    else {
        free(result);
        return NULL;
    }
}

/* The second phase of the lexer tokenizes the identifiers, converting keywords
   into their respective tokens or declaring the identifiers as labels.
   Keyword matching is performed by building a trie. */

typedef struct token_trie {
    char val;
    enum token type;
    struct token_trie  *next;
    struct token_trie *child;
} token_trie;

/* Trie is only built once, so we make it global. */
static token_trie *parser_trie = NULL;

static void insert_into_trie( char *name, enum token type ) {
    char *c;
    token_trie *trie;
    token_trie *trie_parent;
    token_trie *trie_sibling;
    c = name;
    trie_sibling = NULL;
    /* If this is the first entry, we need to make it and move to the next level */
    if( parser_trie == NULL ) {
        parser_trie = (token_trie*)malloc(sizeof(token_trie));
        parser_trie->val = *c;
        parser_trie->next  = NULL;
        parser_trie->child = NULL;
        c++;
        trie_parent = parser_trie;
        trie = NULL;
        if( *c == '\0' ) {
            parser_trie->type = type;
        }
    }
    /* Otherwise we start at the top level */
    else {
        trie_parent = NULL;
        trie = parser_trie;
    }
    while( *c ) {
        if( !trie ) {
            trie = (token_trie*)malloc(sizeof(token_trie));
            trie->val = *c;
            trie->next  = NULL;
            trie->child = NULL;
            c++;
            /* If we have a sibling, attach new node to it */
            if( trie_sibling ) {
                trie_sibling->next = trie;
                trie_sibling = NULL;
            }
            /* Otherwise attach it to the parent */
            else if( trie_parent ) {
                trie_parent->child = trie;
            }
            /* Still have characters to go, so no endpoint */
            if( *c ) {
                trie->type = token_invalid;
                trie_parent = trie;
                trie = trie->child;
            }
            /* Hit the end of the entry, so set the values */
            else {
                trie->type = type;
            }
        }
        /* Node already exists, so traverse */
        else {
            if( trie->val == *c ) {
                c++;
                if( *c == '\0' ) {
                    trie->type = type;
                }
                trie_parent = trie;
                trie = trie->child;
                trie_sibling = NULL;
            }
            else {
                trie_sibling = trie;
                trie = trie->next;
            }
        }
    }
}

static token_trie * trie_lookup( char *name ) {
    token_trie *t;
    char *n;
    t = parser_trie;
    n = name;
    while( *n ) {
        if( !t ) {
            return NULL;
        }
        else {
            if( t->val == *n ) {
                n++;
                if( *n ) {
                    t = t->child;
                }
                else {
                    return t;
                }
            }
            else {
                t = t->next;
            }
        }
    }
    return NULL;
}

static void convert_ident_token( token_list *tok ) {
    token_trie *t;
    t = trie_lookup(tok->ident);
    if( t ) {
        tok->type = t->type;
        free(tok->ident);
        tok->ident = NULL;
    }
    else {
        /* Default to label */
        tok->type = token_label;
    }
}

static void convert_ident_tokens( token_list *tok ) {
    /* Step 1: Build trie */
    /* Registers */
    insert_into_trie("a",    token_reg_a);
    insert_into_trie("b",    token_reg_b);
    insert_into_trie("c",    token_reg_c);
    insert_into_trie("x",    token_reg_x);
    insert_into_trie("y",    token_reg_y);
    insert_into_trie("z",    token_reg_z);
    insert_into_trie("i",    token_reg_i);
    insert_into_trie("j",    token_reg_j);
    /* Register-like keywords */
    insert_into_trie("pc",   token_pc);
    insert_into_trie("sp",   token_sp);
    insert_into_trie("ex",   token_ex);
    insert_into_trie("pop",  token_pop);
    insert_into_trie("peek", token_peek);
    insert_into_trie("push", token_push);
    insert_into_trie("pick", token_pick);
    /* Basic opcodes */
    insert_into_trie("set",  token_set);
    insert_into_trie("add",  token_add);
    insert_into_trie("sub",  token_sub);
    insert_into_trie("mul",  token_mul);
    insert_into_trie("mli",  token_mli);
    insert_into_trie("div",  token_div);
    insert_into_trie("dvi",  token_dvi);
    insert_into_trie("mod",  token_mod);
    insert_into_trie("mdi",  token_mdi);
    insert_into_trie("and",  token_and);
    insert_into_trie("bor",  token_bor);
    insert_into_trie("xor",  token_xor);
    insert_into_trie("shr",  token_shr);
    insert_into_trie("asr",  token_asr);
    insert_into_trie("shl",  token_shl);
    insert_into_trie("ifb",  token_ifb);
    insert_into_trie("ifc",  token_ifc);
    insert_into_trie("ife",  token_ife);
    insert_into_trie("ifn",  token_ifn);
    insert_into_trie("ifg",  token_ifg);
    insert_into_trie("ifa",  token_ifa);
    insert_into_trie("ifl",  token_ifl);
    insert_into_trie("ifu",  token_ifu);
    /* Non-basic opcodes */
    insert_into_trie("jsr",  token_jsr);
    insert_into_trie("hcf",  token_hcf);
    insert_into_trie("int",  token_int);
    insert_into_trie("iag",  token_iag);
    insert_into_trie("ias",  token_ias);
    insert_into_trie("rfi",  token_rfi);
    insert_into_trie("iaq",  token_iaq);
    insert_into_trie("hwn",  token_hwn);
    insert_into_trie("hwq",  token_hwq);
    insert_into_trie("hwi",  token_hwi);
    /* Step 2: Convert tokens */
    while( tok != NULL ) {
        if( tok->type == token_ident ) {
            convert_ident_token(tok);
        }
        tok = tok->next;
    }
}

/*
 * Parsing
 */

typedef uint16_t word;

enum op_value {
    /* Registers */
    opv_a = 0x00,
    opv_b = 0x01,
    opv_c = 0x02,
    opv_x = 0x03,
    opv_y = 0x04,
    opv_z = 0x05,
    opv_i = 0x06,
    opv_j = 0x07,
    /* [Registers */
    opv_br_a = 0x08,
    opv_br_b = 0x09,
    opv_br_c = 0x0a,
    opv_br_x = 0x0b,
    opv_br_y = 0x0c,
    opv_br_z = 0x0d,
    opv_br_i = 0x0e,
    opv_br_j = 0x0f,
    /* [Next word + registers] */
    opv_nw_a = 0x10,
    opv_nw_b = 0x11,
    opv_nw_c = 0x12,
    opv_nw_x = 0x13,
    opv_nw_y = 0x14,
    opv_nw_z = 0x15,
    opv_nw_i = 0x16,
    opv_nw_j = 0x17,
    /* (PUSH / [--SP]) if in b, or (POP / [SP++]) if in a */
    opv_pop_push = 0x18,
    /* Peek / [SP] */
    opv_peek = 0x19,
    /* [SP + next word] / PICK n */
    opv_pick = 0x1a,
    /* SP */
    opv_sp = 0x1b,
    /* PC */
    opv_pc = 0x1c,
    /* EX */
    opv_ex = 0x1d,
    /* [Next word] */
    opv_br_nw = 0x1e,
    /* Next word (literal) */
    opv_nw = 0x1f,
    /* Literal (just first value) (only for a) */
    opv_lit = 0x20
};

enum op_code {
    opc_nonbasic = 0x0,
    opc_set = 0x01,
    opc_add = 0x02,
    opc_sub = 0x03,
    opc_mul = 0x04,
    opc_mli = 0x05,
    opc_div = 0x06,
    opc_dvi = 0x07,
    opc_mod = 0x08,
    opc_mdi = 0x09,
    opc_and = 0x0a,
    opc_bor = 0x0b,
    opc_xor = 0x0c,
    opc_shr = 0x0d,
    opc_asr = 0x0e,
    opc_shl = 0x0f,
    opc_ifb = 0x10,
    opc_ifc = 0x11,
    opc_ife = 0x12,
    opc_ifn = 0x13,
    opc_ifg = 0x14,
    opc_ifa = 0x15,
    opc_ifl = 0x16,
    opc_ifu = 0x17,
    opc_adx = 0x1a,
    opc_sbx = 0x1b,
    opc_sti = 0x1e,
    opc_std = 0x1f
};

enum op_code_nonbasic {
    opc_jsr = 0x01,
    opc_hcf = 0x07,
    opc_int = 0x08,
    opc_iag = 0x09,
    opc_ias = 0x0a,
    opc_rfi = 0x0b,
    opc_iaq = 0x0c,
    opc_hwn = 0x10,
    opc_hwq = 0x11,
    opc_hwi = 0x12
};

typedef struct instruction {
    union {
        struct {
            enum op_code opcode;
            enum op_value a;
            enum op_value b;
        } basic;
        struct {
            enum op_code_nonbasic opcode;
            enum op_value a;
        } nonbasic;
        word literal;
    } v;
    enum {
        basic,
        nonbasic,
        literal
    } tag;
} instruction;

static enum op_code get_op_code( token_list *tok ) {
    switch( tok->type ) {
    case token_set: return opc_set;
    case token_add: return opc_add;
    case token_sub: return opc_sub;
    case token_mul: return opc_mul;
    case token_mli: return opc_mli;
    case token_div: return opc_div;
    case token_dvi: return opc_dvi;
    case token_mod: return opc_mod;
    case token_mdi: return opc_mdi;
    case token_and: return opc_and;
    case token_bor: return opc_bor;
    case token_xor: return opc_xor;
    case token_shr: return opc_shr;
    case token_asr: return opc_asr;
    case token_shl: return opc_shl;
    case token_ifb: return opc_ifb;
    case token_ifc: return opc_ifc;
    case token_ife: return opc_ife;
    case token_ifn: return opc_ifn;
    case token_ifg: return opc_ifg;
    case token_ifa: return opc_ifa;
    case token_ifl: return opc_ifl;
    case token_ifu: return opc_ifu;
    case token_adx: return opc_adx;
    case token_sbx: return opc_sbx;
    case token_sti: return opc_sti;
    case token_std: return opc_std;
    default:
        return opc_nonbasic;
    }
}

static enum op_code_nonbasic get_op_code_nonbasic( token_list *tok ) {
    switch( tok->type ) {
    case token_jsr: return opc_jsr;
    case token_hcf: return opc_hcf;
    case token_int: return opc_int;
    case token_iag: return opc_iag;
    case token_ias: return opc_ias;
    case token_rfi: return opc_rfi;
    case token_iaq: return opc_iaq;
    case token_hwn: return opc_hwn;
    case token_hwq: return opc_hwq;
    case token_hwi: return opc_hwi;
    default:
        fprintf(stderr, "ERROR: Inavlid non-basic operator at line %d, column %d.\n",
                tok->line_number, tok->col_number);
        return opc_jsr;
    }
}

static enum op_value get_op_value( token_list *tok ) {
    switch( tok->type ) {
    /* Registers */
    case token_reg_a: return opv_a;
    case token_reg_b: return opv_b;
    case token_reg_c: return opv_c;
    case token_reg_x: return opv_x;
    case token_reg_y: return opv_y;
    case token_reg_z: return opv_z;
    case token_reg_i: return opv_i;
    case token_reg_j: return opv_j;
    /* Register-like keywords */
    case token_pc:    return opv_pc;
    case token_sp:    return opv_sp;
    case token_ex:    return opv_ex;
    case token_pop:   return opv_pop_push;
    case token_peek:  return opv_peek;
    case token_push:  return opv_pop_push;
    case token_pick:  return opv_pick;
    /* Literals */
    case token_number:
        if( tok->value >= -1 && tok->value <= 30 ) {
            return opv_lit + tok->value + 1;
        }
    default:
        return opv_nw;
    }
}

#define INSTR_LIST_SIZE 5000

typedef struct instruction_list {
    instruction inst[INSTR_LIST_SIZE]; /* We'll start with 5k, if we need more we'll alloc another 5k */
    int length;
    struct instruction_list *next;
} instruction_list;

/* This is used both as the stack of label references that need to be cleaned
   up as well as the list of labels, which makes things slightly confusing. */
typedef struct label_loc_map {
    char *name;
    int loc;
    token_list *tok;
    struct label_loc_map *prev;
} label_loc_map;

static instruction new_instruction_basic( enum op_code o, enum op_value b, enum op_value a ) {
    instruction result;
    result.tag = basic;
    result.v.basic.opcode = o;
    result.v.basic.a = a;
    result.v.basic.b = b;
    return result;
}

static instruction new_instruction_nonbasic( enum op_code_nonbasic o, enum op_value a ) {
    instruction result;
    result.tag = nonbasic;
    result.v.nonbasic.opcode = o;
    result.v.nonbasic.a = a;
    return result;
}

static instruction new_instruction_literal( word value ) {
    instruction result;
    result.tag = literal;
    result.v.literal = value;
    return result;
}

static instruction_list * new_instruction_list() {
    instruction_list *result;
    result = (instruction_list*)malloc(sizeof(instruction_list));
    result->length = 0;
    result->next = NULL;
    return result;
}

static label_loc_map * insert_into_label_loc_map( label_loc_map *map, char *name, int loc, token_list *tok ) {
    label_loc_map *result;
    result = (label_loc_map*)malloc(sizeof(label_loc_map));
    result->name = name;
    result->loc = loc;
    result->tok = tok;
    result->prev = map;
    return result;
}

static instruction_list * insert_instruction( instruction_list *l, instruction i ) {
    instruction_list *list;
    if( !l ) {
        l = new_instruction_list();
    }
    list = l;
    /* Get last instr list to append */
    while( list->next ) {
        list = list->next;
    }
    /* Create new list if current list is full */
    if( list->length == INSTR_LIST_SIZE ) {
        list->next = new_instruction_list();
        list = list->next;
    }
    list->inst[list->length] = i;
    list->length++;
    return l;
}

static instruction_list * insert_literal( instruction_list *list, enum op_value val, word lit ) {
    /* Need to check the val to ensure that the lit actually needs to be inserted */
    switch( val ) {
    case opv_nw_a:
    case opv_nw_b:
    case opv_nw_c:
    case opv_nw_x:
    case opv_nw_y:
    case opv_nw_z:
    case opv_nw_i:
    case opv_nw_j:
    case opv_br_nw:
    case opv_nw:
        /* Things that require a literal value */
        return insert_instruction(list, new_instruction_literal(lit));
    default:
        /* Things that don't */
        return list;
    }
}

static label_loc_map * label_lookup( token_list *tok, label_loc_map *map ) {
    while( map ) {
        /* Found a match */
        if( strcmp(tok->ident, map->name) == 0 ) {
            return map;
        }
        /* Else */
        map = map->prev;
    }
    /* No matches */
    return NULL;
}

static int get_last_instr_loc( instruction_list *list ) {
    int result;
    if( !list ) {
        return 0;
    }
    result = 0;
    while( list->next ) {
        result = result + INSTR_LIST_SIZE;
        list = list->next;
    }
    return result + list->length;
}

static int parse_label( instruction_list *list, token_list **tp, label_loc_map **ltp ) {
    token_list *tok;
    label_loc_map *match;
    int loc;
    tok = *tp;
    if( !tok->next ) {
        fprintf(stderr, "ERROR: Syntax error: Expected label, found EOF at line %d, column %d.\n",
                tok->line_number, tok->col_number);
        return -1;
    }
    tok = tok->next;
    if( tok->type != token_label ) {
        fprintf(stderr, "ERROR: Syntax error: Expected label at line %d, column %d.\n",
                tok->line_number, tok->col_number);
        return -1;
    }
    match = label_lookup(tok, *ltp);
    if( match ) {
        fprintf(stderr, "ERROR: Syntax error: Duplicate labels at line %d, column %d and line %d, column %d.\n",
                match->tok->line_number, match->tok->col_number, tok->line_number, tok->col_number);
        return -1;
    }
    /* Parse succeeds, move up the token list and insert new label into map */
    *tp = (*tp)->next;
    loc = get_last_instr_loc(list);
    *ltp = insert_into_label_loc_map(*ltp, tok->ident, loc, tok);
    return 0;
}

static int parse_bracket( token_list **tp, label_loc_map **utp, enum op_value *val, word *lit, int loc ) {
    enum token type;
    if( !(*tp)->next ) {
        fprintf(stderr, "ERROR: Syntax error: Unexpected EOF after '[' at line %d, column %d.\n",
                (*tp)->line_number, (*tp)->col_number);
        return -1;
    }
    *tp = (*tp)->next;
    /* Number/label first? */
    if( (*tp)->type == token_number || (*tp)->type == token_label ) {
        if( (*tp)->type == token_number ) {
            *val = opv_br_nw;
            *lit = (*tp)->value;
        }
        else { /* Label */
            if( !label_lookup(*tp, *utp) ) {
                *utp = insert_into_label_loc_map(*utp, (*tp)->ident, loc, *tp);
            }
            *val = opv_br_nw;
        }
        if( !(*tp)->next ) {
            fprintf(stderr, "ERROR: Syntax error: Unexpected EOF after number at line %d, column %d.\n",
                    (*tp)->line_number, (*tp)->col_number);
            return -1;
        }
        /* [Lit + Reg] */
        if( (*tp)->next->type == token_plus ) {
            *tp = (*tp)->next;
            if( !(*tp)->next ) {
                fprintf(stderr, "ERROR: Syntax error: Unexpected EOF after '+' at line %d, column %d.\n",
                        (*tp)->line_number, (*tp)->col_number);
                return -1;
            }
            *tp = (*tp)->next;
            switch( (*tp)->type ) {
            case token_reg_a: *val = opv_nw_a; break;
            case token_reg_b: *val = opv_nw_b; break;
            case token_reg_c: *val = opv_nw_c; break;
            case token_reg_x: *val = opv_nw_x; break;
            case token_reg_y: *val = opv_nw_y; break;
            case token_reg_z: *val = opv_nw_z; break;
            case token_reg_i: *val = opv_nw_i; break;
            case token_reg_j: *val = opv_nw_j; break;
            default:
                fprintf(stderr, "ERROR: Syntax error: Expected register at line %d, column %d.\n",
                        (*tp)->line_number, (*tp)->col_number);
                return -1;
            }
        }
    }
    /* Register first */
    else {
        switch( (*tp)->type ) {
        case token_reg_a:
        case token_reg_b:
        case token_reg_c:
        case token_reg_x:
        case token_reg_y:
        case token_reg_z:
        case token_reg_i:
        case token_reg_j:
            type = (*tp)->type;
            break;
        default:
            fprintf(stderr, "ERROR: Syntax error: Expected register at line %d, column %d.\n",
                    (*tp)->line_number, (*tp)->col_number);
            return -1;
        }
        if( !(*tp)->next ) {
            fprintf(stderr, "ERROR: Syntax error: Unexpected EOF after register at line %d, column %d.\n",
                    (*tp)->line_number, (*tp)->col_number);
            return -1;
        }
        if( (*tp)->next->type == token_plus ) {
            *tp = (*tp)->next;
            if( !(*tp)->next ) {
                fprintf(stderr, "ERROR: Syntax error: Unexpected EOF after '+' at line %d, column %d.\n",
                        (*tp)->line_number, (*tp)->col_number);
                return -1;
            }
            *tp = (*tp)->next;
            if( (*tp)->type == token_number ) {
                *lit = (*tp)->value;
            }
            else if( (*tp)->type == token_label ) {
                if( !label_lookup(*tp, *utp) ) {
                    *utp = insert_into_label_loc_map(*utp, (*tp)->ident, loc, *tp);
                }
            }
            else {
                fprintf(stderr, "ERROR: Syntax error: Expected number or label at line %d, column %d.\n",
                        (*tp)->line_number, (*tp)->col_number);
                return -1;
            }
            switch( type ) {
            case token_reg_a: *val = opv_nw_a; break;
            case token_reg_b: *val = opv_nw_b; break;
            case token_reg_c: *val = opv_nw_c; break;
            case token_reg_x: *val = opv_nw_x; break;
            case token_reg_y: *val = opv_nw_y; break;
            case token_reg_z: *val = opv_nw_z; break;
            case token_reg_i: *val = opv_nw_i; break;
            case token_reg_j: *val = opv_nw_j; break;
            default: /* Shouldn't be possible */
                break;
            }
        }
        else {
            switch( type ) {
            case token_reg_a: *val = opv_br_a; break;
            case token_reg_b: *val = opv_br_b; break;
            case token_reg_c: *val = opv_br_c; break;
            case token_reg_x: *val = opv_br_x; break;
            case token_reg_y: *val = opv_br_y; break;
            case token_reg_z: *val = opv_br_z; break;
            case token_reg_i: *val = opv_br_i; break;
            case token_reg_j: *val = opv_br_j; break;
            default: /* Shouldn't be possible */
                break;
            }
        }
    }
    /* Check for right bracket */
    if( !(*tp)->next ) {
        fprintf(stderr, "ERROR: Syntax error: Unexpected EOF, expected ']' after token at line %d, column %d.\n",
                (*tp)->line_number, (*tp)->col_number);
        return -1;
    }
    *tp = (*tp)->next;
    if( (*tp)->type != token_rightbracket ) {
        fprintf(stderr, "ERROR: Syntax error: Expected ']' at line %d, column %d.\n",
                (*tp)->line_number, (*tp)->col_number);
        return -1;
    }
    /* Success */
    return 0;
}

static int parse_value( token_list **tp, label_loc_map **utp, enum op_value *val, word *lit, int loc ) {
    if( !(*tp) ) {
        fprintf(stderr, "ERROR: Syntax error: Unexpected EOF.\n");
        return -1;
    }
    /* First check for brackets */
    /**tp = (*tp)->next;*/
    if( (*tp)->type == token_leftbracket ) {
        return parse_bracket(tp, utp, val, lit, loc);
    }
    else {
        *val = get_op_value(*tp);
        /* Labels get placeholder values for now, and are later cleaned up */
        if( (*tp)->type == token_label ) {
            if( !label_lookup(*tp, *utp) ) {
                *utp = insert_into_label_loc_map(*utp, (*tp)->ident, loc, *tp);
            }
        }
        /* Have to check if we're given a literal that's larger than 0x1f */
        else if( (*tp)->type == token_number && *val == opv_nw ) {
            *lit = (*tp)->value;
        }
        return 0;
    }
}

static int parse_basic( instruction_list *list, token_list **tp, label_loc_map **utp ) {
    enum op_value val1, val2;
    word lit1, lit2;
    enum op_code op;
    op = get_op_code(*tp);
    /* Check for b value */
    if( !(*tp)->next ) {
        fprintf(stderr, "ERROR: Syntax error: Unexpected EOF after line %d, column %d.\n",
                (*tp)->line_number, (*tp)->col_number);
        return -1;
    }
    *tp = (*tp)->next;
    if( parse_value(tp, utp, &val1, &lit1, get_last_instr_loc(list)+1) ) {
        return -1;
    }
    /* Check for comma */
    if( !(*tp) ) {
        /* Error will be reported in parse_value if this is the case */
        return -1;
    }
    else if( !(*tp)->next ) {
        fprintf(stderr, "ERROR: Syntax error: Unexpected EOF after line %d, column %d.\n",
                (*tp)->line_number, (*tp)->col_number);
        return -1;
    }
    *tp = (*tp)->next;
    if( (*tp)->type != token_comma ) {
        fprintf(stderr, "ERROR: Syntax error: Expected ',' at line %d, column %d.\n",
                (*tp)->line_number, (*tp)->col_number);
        return -1;
    }
    /* Check for a value */
    else if( !(*tp)->next ) {
        fprintf(stderr, "ERROR: Syntax error: Expected token after ',' at line %d, column %d.\n",
                (*tp)->line_number, (*tp)->col_number);
        return -1;
    }
    else {
        *tp = (*tp)->next;
        if( parse_value(tp, utp, &val2, &lit2, get_last_instr_loc(list)+2) ) {
            return -1;
        }
        list = insert_instruction(list, new_instruction_basic(op, val1, val2));
        /* a is the second value parsed, but the first written out */
        list = insert_literal(list, val2, lit2);
        insert_literal(list, val1, lit1);
        return 0;
    }
}

static int parse_nonbasic( instruction_list *list, token_list **tp, label_loc_map **utp ) {
    enum op_value val;
    word lit;
    enum op_code_nonbasic op;
    op = get_op_code_nonbasic(*tp);
    if( !(*tp)->next ) {
        fprintf(stderr, "ERROR: Syntax error: Unexpected EOF after line %d, column %d.\n",
                (*tp)->line_number, (*tp)->col_number);
        return -1;
    }
    *tp = (*tp)->next;
    if( parse_value(tp, utp, &val, &lit, get_last_instr_loc(list)+1) ) {
        return -1;
    }
    /* Only a value in nonbasic ops, so done here */
    else {
        list = insert_instruction(list, new_instruction_nonbasic(op, val));
        insert_literal(list, val, lit);
        return 0;
    }
}

static void free_label_loc_map( label_loc_map *map ) {
    label_loc_map *prev;
    if( map ) {
        prev = map->prev;
        free(map);
        free_label_loc_map(prev);
    }
}

static label_loc_map * update_label( instruction_list *list, label_loc_map *to_update, label_loc_map *symbols ) {
    label_loc_map *entry, *prev;
    int loc;
    entry = label_lookup(to_update->tok, symbols);
    if( !entry ) {
        fprintf(stderr, "ERROR: Syntax error: Non-existant label '%s' referenced at line %d, column %d.\n",
                to_update->name, to_update->tok->line_number, to_update->tok->col_number);
        free_label_loc_map(to_update);
        return NULL;
    }
    else {
        loc = to_update->loc;
        while( loc >= INSTR_LIST_SIZE && list ) {
            loc -= INSTR_LIST_SIZE;
            list = list->next;
        }
        if( !list ) {
            fprintf(stderr, "ERROR: Bad loc: %d.\n", to_update->loc);
            free_label_loc_map(to_update);
            return NULL;
        }
        else {
            list->inst[loc].v.literal = entry->loc;
            prev = to_update->prev;
            free(to_update);
            return prev;
        }
    }
}

static instruction_list * parse( token_list *tok ) {
    instruction_list *result;
    label_loc_map *update_list, *label_table;
    result = new_instruction_list();
    update_list = label_table = NULL;
    while( tok ) {
        switch( tok->type ) {
        /* Labels */
        case token_colon:
            if( parse_label(result, &tok, &label_table) ) {
                return NULL;
            }
            break;
        /* Basic opcodes */
        case token_set:
        case token_add:
        case token_sub:
        case token_mul:
        case token_div:
        case token_mod:
        case token_and:
        case token_bor:
        case token_xor:
        case token_shr:
        case token_shl:
        case token_ifb:
        case token_ifc:
        case token_ife:
        case token_ifn:
        case token_ifg:
        case token_ifa:
        case token_ifl:
        case token_ifu:
        case token_adx:
        case token_sbx:
        case token_sti:
        case token_std:
            if( parse_basic(result, &tok, &update_list) ) {
                return NULL;
            }
            break;
        /* Non-basic opcodes */
        case token_jsr:
        case token_hcf:
        case token_int:
        case token_iag:
        case token_ias:
        case token_rfi:
        case token_iaq:
        case token_hwn:
        case token_hwq:
        case token_hwi:
            if( parse_nonbasic(result, &tok, &update_list) ) {
                return NULL;
            }
            break;
        /* Error otherwise */
        default:
            fprintf(stderr, "ERROR: Syntax error: Invalid start of statement at line %d, column %d.\n",
                    tok->line_number, tok->col_number);
            return NULL;
        }
        tok = tok->next;
    }
    /* Assuming no errors, we have to next update the labels with locations */
    if( result ) {
        while( update_list ) {
            update_list = update_label(result, update_list, label_table);
        }
    }
    return result;
}

static void output_instruction_list( instruction_list *list, char *filename ) {
    word output;
    instruction inst;
    FILE *fp;
    int i;
    unsigned char bytes[2];
    fp = fopen(filename, "wb");
    if( !fp ) {
        fprintf(stderr, "ERROR: Failed to create output file '%s'.\n", filename);
    }
    else {
        while( list ) {
            for( i = 0; i < INSTR_LIST_SIZE && i < list->length; i++ ) {
                inst = list->inst[i];
                if( inst.tag == literal ) {
                    output = inst.v.literal;
                }
                else if( inst.tag == basic ) {
                    output  = inst.v.basic.opcode;
                    output |= inst.v.basic.b <<  5;
                    output |= inst.v.basic.a << 10;
                }
                else {
                    output  = inst.v.nonbasic.opcode <<  5;
                    output |= inst.v.nonbasic.a      << 10;
                }
                /* Force byte order */
                bytes[0] = (unsigned char)((output >> 8) & 0xff);
                bytes[1] = (unsigned char)(output & 0xff);
                fwrite(bytes, 1, 1, fp);
                fwrite(bytes+1, 1, 1, fp);
            }
            list = list->next;
        }
    }
    fclose(fp);
}

int main( int argc, char **argv ) {
    token_list *toks;
    instruction_list *instr;
    FILE *source;
    if( argc < 3 ) {
        printf("Usage: %s source output\n", argv[0]);
        return 0;
    }
    else {
        source = fopen(argv[1], "r");
        if( !source ) {
            fprintf(stderr, "ERROR: Failed to open source file '%s'.\n", argv[1]);
            return 0;
        }
        else {
            toks = build_token_list_f(source);
            fclose(source);
            if( toks ) {
                convert_ident_tokens(toks);
                instr = parse(toks);
                if( instr ) {
                    output_instruction_list(instr, argv[2]);
                }
            }
            return 0;
        }
    }
}
