// Word tables for the syntax highlighters
// This is free and unencumbered software released into the public domain.
//
// Tables are searched by bisection, so each must be sorted in byte order
// (a unit test checks). Tables for case-insensitive languages are
// lowercase.

// C and C++

static Str ckeywords[] = {
    "NULL", "_Alignas", "_Alignof", "_Atomic", "_Generic", "_Noreturn",
    "_Pragma", "_Static_assert", "_Thread_local", "__asm", "__asm__",
    "__attribute", "__attribute__", "__cdecl", "__declspec",
    "__extension__", "__inline", "__inline__", "__restrict", "__stdcall",
    "__thread", "__typeof", "__typeof__", "__volatile__", "alignas",
    "alignof", "asm", "auto", "break", "case", "const", "constexpr",
    "continue", "default", "do", "else", "enum", "extern", "false", "for",
    "goto", "if", "inline", "nullptr", "register", "restrict", "return",
    "sizeof", "static", "static_assert", "struct", "switch", "thread_local",
    "true", "typedef", "typeof", "typeof_unqual", "union", "volatile",
    "while",
};

static Str cppkeywords[] = {
    "NULL", "_Alignas", "_Alignof", "_Atomic", "_Generic", "_Noreturn",
    "_Pragma", "_Static_assert", "_Thread_local", "__asm", "__asm__",
    "__attribute", "__attribute__", "__cdecl", "__declspec",
    "__extension__", "__inline", "__inline__", "__restrict", "__stdcall",
    "__thread", "__typeof", "__typeof__", "__volatile__", "alignas",
    "alignof", "asm", "auto", "break", "case", "catch", "class", "co_await",
    "co_return", "co_yield", "concept", "const", "const_cast", "consteval",
    "constexpr", "constinit", "continue", "decltype", "default", "delete",
    "do", "dynamic_cast", "else", "enum", "explicit", "export", "extern",
    "false", "for", "friend", "goto", "if", "inline", "mutable",
    "namespace", "new", "noexcept", "nullptr", "operator", "private",
    "protected", "public", "register", "reinterpret_cast", "requires",
    "restrict", "return", "sizeof", "static", "static_assert",
    "static_cast", "struct", "switch", "template", "this", "thread_local",
    "throw", "true", "try", "typedef", "typeid", "typename", "typeof",
    "typeof_unqual", "union", "using", "virtual", "volatile", "while",
};

// Besides these, names ending in _t, and capitalized names like Arena
// (not followed by a parenthesis), are types.
static Str ctypes[] = {
    "BOOL", "BYTE", "CHAR", "DWORD", "FILE", "HANDLE", "HINSTANCE",
    "HMODULE", "HWND", "LONG", "LPARAM", "LPCSTR", "LPCWSTR", "LPSTR",
    "LPVOID", "LPWSTR", "LRESULT", "PVOID", "UINT", "ULONG", "WCHAR",
    "WORD", "WPARAM", "_Bool", "_Complex", "_Float16", "b32", "b8", "bool",
    "byte", "c16", "c32", "c8", "char", "double", "f32", "f64", "float",
    "i16", "i32", "i64", "i8", "int", "isize", "iz", "jmp_buf", "long",
    "s16", "s8", "short", "signed", "u16", "u32", "u64", "u8", "unsigned",
    "uptr", "usize", "uz", "va_list", "void",
};

static Str ctdefs[]   = {"enum", "struct", "union"};
static Str cpptdefs[] = {"class", "enum", "struct", "typename", "union"};
static Str cprefixes[] = {"L", "U", "u", "u8"};

static Str glslkeywords[] = {
    "attribute", "break", "case", "centroid", "const", "continue",
    "default", "discard", "do", "else", "false", "flat", "for", "highp",
    "if", "in", "inout", "invariant", "layout", "lowp", "mediump",
    "noperspective", "out", "precision", "return", "smooth", "struct",
    "switch", "true", "uniform", "varying", "while",
};

static Str glsltypes[] = {
    "bool", "bvec2", "bvec3", "bvec4", "double", "dvec2", "dvec3", "dvec4",
    "float", "int", "ivec2", "ivec3", "ivec4", "mat2", "mat2x2", "mat2x3",
    "mat2x4", "mat3", "mat3x2", "mat3x3", "mat3x4", "mat4", "mat4x2",
    "mat4x3", "mat4x4", "sampler1D", "sampler2D", "sampler2DShadow",
    "sampler3D", "samplerCube", "uint", "uvec2", "uvec3", "uvec4", "vec2",
    "vec3", "vec4", "void",
};

// Java, JavaScript, Go

static Str javakeywords[] = {
    "abstract", "assert", "break", "case", "catch", "class", "const",
    "continue", "default", "do", "else", "enum", "extends", "false",
    "final", "finally", "for", "goto", "if", "implements", "import",
    "instanceof", "interface", "native", "new", "null", "package",
    "private", "protected", "public", "record", "return", "static",
    "strictfp", "super", "switch", "synchronized", "this", "throw",
    "throws", "transient", "true", "try", "var", "volatile", "while",
    "yield",
};

static Str javatypes[] = {
    "boolean", "byte", "char", "double", "float", "int", "long", "short",
    "void",
};

static Str javadefs[]  = {"class", "enum", "interface", "record"};
static Str javatdefs[] = {"new"};

static Str jskeywords[] = {
    "async", "await", "break", "case", "catch", "class", "const",
    "continue", "debugger", "default", "delete", "do", "else", "export",
    "extends", "false", "finally", "for", "function", "if", "import", "in",
    "instanceof", "let", "new", "null", "of", "return", "static", "super",
    "switch", "this", "throw", "true", "try", "typeof", "undefined", "var",
    "void", "while", "with", "yield",
};

static Str jsdefs[] = {"class", "function"};

static Str gokeywords[] = {
    "break", "case", "chan", "const", "continue", "default", "defer",
    "else", "fallthrough", "false", "for", "func", "go", "goto", "if",
    "import", "interface", "iota", "map", "nil", "package", "range",
    "return", "select", "struct", "switch", "true", "type", "var",
};

static Str gotypes[] = {
    "any", "bool", "byte", "complex128", "complex64", "error", "float32",
    "float64", "int", "int16", "int32", "int64", "int8", "rune", "string",
    "uint", "uint16", "uint32", "uint64", "uint8", "uintptr",
};

static Str godefs[] = {"func", "type"};

// Keywords that are operands, after which "/" divides rather than
// begins a regular expression.
static Str valuekeywords[] = {
    "false", "nil", "null", "self", "super", "this", "true", "undefined",
};

// Scripting languages

static Str pykeywords[] = {
    "False", "None", "True", "and", "as", "assert", "async", "await",
    "break", "class", "continue", "def", "del", "elif", "else", "except",
    "finally", "for", "from", "global", "if", "import", "in", "is",
    "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try",
    "while", "with", "yield",
};

static Str pytypes[] = {
    "bool", "bytearray", "bytes", "complex", "dict", "float", "frozenset",
    "int", "list", "object", "set", "str", "tuple", "type",
};

static Str pydefs[] = {"class", "def"};

static Str pyprefixes[] = {
    "B", "F", "R", "U", "b", "br", "f", "fr", "r", "rb", "rf", "u",
};

static Str luakeywords[] = {
    "and", "break", "do", "else", "elseif", "end", "false", "for",
    "function", "goto", "if", "in", "local", "nil", "not", "or", "repeat",
    "return", "then", "true", "until", "while",
};

static Str luadefs[] = {"function"};

static Str phpkeywords[] = {
    "abstract", "and", "array", "as", "break", "callable", "case", "catch",
    "class", "clone", "const", "continue", "declare", "default", "do",
    "echo", "else", "elseif", "empty", "enddeclare", "endfor", "endforeach",
    "endif", "endswitch", "endwhile", "eval", "exit", "extends", "false",
    "final", "finally", "fn", "for", "foreach", "function", "global",
    "goto", "if", "implements", "include", "include_once", "instanceof",
    "insteadof", "interface", "isset", "list", "match", "namespace", "new",
    "null", "or", "print", "private", "protected", "public", "readonly",
    "require", "require_once", "return", "static", "switch", "throw",
    "trait", "true", "try", "unset", "use", "var", "while", "xor", "yield",
};

static Str phpdefs[] = {"class", "function"};

static Str perlkeywords[] = {
    "and", "cmp", "continue", "do", "else", "elsif", "eq", "for", "foreach",
    "ge", "gt", "if", "last", "le", "local", "lt", "my", "ne", "next", "no",
    "not", "or", "our", "package", "redo", "require", "return", "sub",
    "unless", "until", "use", "while", "xor",
};

static Str perldefs[] = {"sub"};

static Str rubykeywords[] = {
    "BEGIN", "END", "alias", "and", "begin", "break", "case", "class",
    "def", "do", "else", "elsif", "end", "ensure", "false", "for", "if",
    "in", "module", "next", "nil", "not", "or", "redo", "rescue", "retry",
    "return", "self", "super", "then", "true", "undef", "unless", "until",
    "when", "while", "yield",
};

static Str rubydefs[] = {"class", "def", "module"};

static Str juliakeywords[] = {
    "abstract", "baremodule", "begin", "break", "catch", "const",
    "continue", "do", "else", "elseif", "end", "export", "false", "finally",
    "for", "function", "global", "if", "import", "in", "isa", "let",
    "local", "macro", "module", "mutable", "nothing", "primitive", "quote",
    "return", "struct", "true", "try", "using", "where", "while",
};

static Str juliadefs[] = {"function", "macro", "module", "struct"};

static Str matlabkeywords[] = {
    "break", "case", "catch", "classdef", "continue", "do", "else",
    "elseif", "end", "end_try_catch", "end_unwind_protect", "endfor",
    "endfunction", "endif", "endswitch", "endwhile", "for", "function",
    "global", "if", "otherwise", "parfor", "persistent", "return", "switch",
    "try", "until", "unwind_protect", "unwind_protect_cleanup", "while",
};

static Str matlabdefs[] = {"function"};

static Str sqlkeywords[] = {
    "add", "all", "alter", "and", "as", "asc", "begin", "between", "by",
    "case", "cast", "check", "collate", "column", "commit", "constraint",
    "create", "cross", "default", "delete", "desc", "distinct", "drop",
    "else", "end", "exists", "foreign", "from", "full", "glob", "group",
    "having", "if", "in", "index", "inner", "insert", "into", "is", "join",
    "key", "left", "like", "limit", "natural", "not", "null", "offset",
    "on", "or", "order", "outer", "primary", "references", "replace",
    "right", "rollback", "select", "set", "table", "then", "transaction",
    "trigger", "union", "unique", "update", "using", "values", "view",
    "when", "where", "with", "without",
};

static Str sqltypes[] = {
    "blob", "boolean", "char", "datetime", "decimal", "double", "float",
    "int", "integer", "numeric", "real", "text", "varchar",
};

static Str basickeywords[] = {
    "and", "as", "call", "case", "close", "cls", "common", "const", "data",
    "declare", "def", "dim", "do", "else", "elseif", "end", "exit", "for",
    "function", "gosub", "goto", "if", "input", "is", "let", "locate",
    "loop", "mod", "next", "not", "on", "open", "or", "print", "randomize",
    "read", "redim", "restore", "return", "select", "shared", "static",
    "step", "stop", "sub", "swap", "then", "to", "type", "until", "wend",
    "while", "xor",
};

static Str basictypes[] = {"double", "integer", "long", "single", "string"};
static Str basicdefs[]  = {"function", "sub", "type"};

static Str gnuplotkeywords[] = {
    "axes", "border", "boxes", "circles", "every", "fill", "for", "in",
    "lines", "linespoints", "linestyle", "linetype", "linewidth", "lt",
    "lw", "noborder", "notitle", "output", "palette", "plot", "points",
    "pointtype", "pt", "radius", "replot", "set", "smooth", "solid",
    "splot", "style", "terminal", "title", "transparent", "unset", "using",
    "with", "xlabel", "xrange", "ylabel", "yrange",
};

static Str vimkeywords[] = {
    "au", "augroup", "autocmd", "call", "cexpr", "colorscheme", "command",
    "echo", "edit", "else", "elseif", "endfor", "endfunction", "endif",
    "endwhile", "exe", "execute", "filetype", "for", "function", "if",
    "imap", "inoremap", "let", "map", "nmap", "nnoremap", "noremap",
    "normal", "return", "set", "setlocal", "silent", "source", "syntax",
    "update", "vmap", "vnoremap", "while",
};

static Str jsonkeywords[] = {"false", "null", "true"};

// Lisp family: special forms and macros at the head of a form

static Str lispheads[] = {
    "and", "begin", "block", "case", "case-lambda", "catch", "cl-block",
    "cl-case", "cl-destructuring-bind", "cl-do", "cl-dolist", "cl-dotimes",
    "cl-ecase", "cl-etypecase", "cl-flet", "cl-labels", "cl-letf",
    "cl-loop", "cl-macrolet", "cl-return", "cl-return-from", "cl-typecase",
    "cond", "condition-case", "declare", "delay", "destructuring-bind",
    "do", "dolist", "dotimes", "ecase", "eval-when", "eval-when-compile",
    "flet", "fn", "function", "go", "handler-case", "if", "if-let",
    "if-not", "ignore-errors", "interactive", "labels", "lambda", "let",
    "let*", "letrec", "loop", "macrolet", "multiple-value-bind",
    "named-lambda", "ns", "or", "pcase", "pcase-let", "prog1", "prog2",
    "progn", "provide", "quote", "recur", "require", "return",
    "return-from", "save-excursion", "save-match-data", "save-restriction",
    "set!", "setf", "setq", "tagbody", "the", "throw", "unless",
    "unwind-protect", "when", "when-let", "while", "while-let",
    "with-current-buffer", "with-temp-buffer",
};

// Heads that define a name, which is highlighted as a definition
static Str lispdefs[] = {
    "cl-defgeneric", "cl-defmacro", "cl-defmethod", "cl-defstruct",
    "cl-defsubst", "cl-defun", "def", "defadvice", "defalias", "defclass",
    "defconst", "defcustom", "defface", "defgeneric", "defgroup", "define",
    "define-derived-mode", "define-minor-mode", "define-syntax", "defmacro",
    "defmethod", "defmulti", "defn", "defn-", "defpackage", "defparameter",
    "defprotocol", "defrecord", "defservlet", "defstruct", "defsubst",
    "deftest", "deftype", "defun", "defvar", "defvar-local", "ert-deftest",
};

static Str watdefs[]  = {"func", "global", "module"};
static Str wattypes[] = {
    "anyfunc", "externref", "f32", "f64", "funcref", "i32", "i64", "v128",
};

// Assembly

static Str nasmdirectives[] = {
    "absolute", "align", "alignb", "bits", "common", "cpu", "db", "dd",
    "default", "do", "dq", "dt", "dw", "dy", "dz", "endstruc", "equ",
    "export", "extern", "global", "iend", "incbin", "istruc", "org", "resb",
    "resd", "reso", "resq", "rest", "resw", "resy", "resz", "section",
    "segment", "struc", "times", "use16", "use32", "use64",
};

static Str nasmsizes[] = {
    "abs", "byte", "dword", "far", "near", "oword", "ptr", "qword", "rel",
    "short", "strict", "tword", "word", "yword", "zword",
};

// Instruction prefixes, followed by another mnemonic
static Str asmprefixes[] = {
    "lock", "rep", "repe", "repne", "repnz", "repz",
};

// Numbered registers (r8d, xmm0, ...) are recognized separately
static Str x86regs[] = {
    "ah", "al", "ax", "bh", "bl", "bp", "bpl", "bx", "ch", "cl", "cs", "cx",
    "dh", "di", "dil", "dl", "ds", "dx", "eax", "ebp", "ebx", "ecx", "edi",
    "edx", "eip", "es", "esi", "esp", "fs", "gs", "ip", "rax", "rbp", "rbx",
    "rcx", "rdi", "rdx", "rip", "rsi", "rsp", "si", "sil", "sp", "spl",
    "ss",
};

static Str a64regs[] = {"fp", "lr", "pc", "sp", "wsp", "wzr", "xzr"};

// Shell, make, batch

static Str shkeywords[] = {
    "alias", "case", "cd", "declare", "do", "done", "echo", "elif", "else",
    "esac", "eval", "exec", "exit", "export", "fi", "for", "function", "if",
    "in", "local", "printf", "read", "readonly", "return", "select", "set",
    "shift", "shopt", "source", "then", "time", "trap", "unalias", "unset",
    "until", "while",
};

static Str makedirectives[] = {
    "-include", "define", "else", "endef", "endif", "export", "ifdef",
    "ifeq", "ifndef", "ifneq", "include", "override", "sinclude",
    "undefine", "unexport", "vpath",
};

static Str batkeywords[] = {
    "call", "cd", "copy", "defined", "del", "do", "echo", "else",
    "endlocal", "equ", "errorlevel", "exist", "exit", "for", "geq", "goto",
    "gtr", "if", "in", "leq", "lss", "md", "mkdir", "move", "neq", "not",
    "pause", "popd", "pushd", "rd", "ren", "rmdir", "set", "setlocal",
    "shift", "start", "title", "type",
};

static Str yamlkeywords[] = {"false", "null", "true"};

static Str basicrem[] = {"rem"};  // also for batch files
