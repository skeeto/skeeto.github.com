// Syntax highlighter tests

static Str hlrender(Arena *perm, Str lang, Str code, Arena scratch, b32 *known = 0)
{
    Buf b(perm, 256);
    b32 ok = highlight(&b, lang, code, scratch);
    if (known) *known = ok;
    return finish(&b);
}

// Class of the first span wrapping exactly tok (escaped), or "" if none.
static Str hlclassof(Str html, Str tok)
{
    for (iz off = 0; off < html.len;) {
        iz i = find(cuthead(html, off), tok);
        if (i < 0) {
            break;
        }
        i += off;
        Str before = takehead(html, i);
        Str after  = cuthead(html, i+tok.len);
        if (endswith(before, "\">") && startswith(after, "</span>")) {
            before = cuttail(before, 2);
            iz k = before.len;
            for (; k>0 && before[k-1]!='"'; k--) {}
            if (endswith(takehead(before, k), "<span class=\"")) {
                return cuthead(before, k);
            }
        }
        off = i + 1;
    }
    return "";
}

static iz hlcount(Str s, Str needle)
{
    iz n = 0;
    for (iz i; (i = find(s, needle)) >= 0; n++) {
        s = cuthead(s, i+needle.len);
    }
    return n;
}

static Str hltags[] = {
    "aarch64", "att", "bash", "bat", "c", "c++", "cl", "clojure", "cpp", "diff",
    "elisp", "glsl", "gnuplot", "go", "html", "ini", "java",
    "javascript", "js", "json", "julia", "lisp", "make", "makefile",
    "matlab", "nasm", "obj", "octave", "pc", "perl", "php", "py", "python",
    "qbasic", "ruby", "scheme", "sh", "sql", "udiff", "vim", "wat", "xml",
    "yaml", "lua",
};

static void test_highlight(Test *t)
{
    Arena perm    = t->perm;
    Arena scratch = perm;
    perm.end    -= 1<<20;
    scratch.beg  = perm.end;

    // Token classes: {language, code, token as escaped HTML, class}
    struct { Str lang, code, tok, cls; } cases[] = {
        {"c", "int main(void) { return 0; }", "int", "t"},
        {"c", "int main(void) { return 0; }", "main", "f"},
        {"c", "int main(void) { return 0; }", "return", "k"},
        {"c", "int main(void) { return 0; }", "0", "m"},
        {"c", "x = 1; // don't\ny = 'a';", "// don't", "c"},
        {"c", "x = 1; // don't\ny = 'a';", "'a'", "s"},
        {"c", "c = '\\'';", "'\\''", "s"},
        {"c", "p = \"say \\\"hi\\\"\";", "\"say \\\"hi\\\"\"", "s"},
        {"c", "/* multi\nline */ int x;", "/* multi\nline */", "c"},
        {"c", "#include <stdio.h>", "#include", "p"},
        {"c", "#include <stdio.h>", "&lt;stdio.h&gt;", "s"},
        {"c", "#define MAX(a, b) ((a) > (b) ? (a) : (b))", "MAX", "f"},
        {"c", "uint32_t x = 0x9b60933458e17dULL;", "uint32_t", "t"},
        {"c", "uint32_t x = 0x9b60933458e17dULL;", "0x9b60933458e17dULL", "m"},
        {"c", "float f = 1.5e-3f;", "1.5e-3f", "m"},
        {"c", "wchar_t *s = L\"wide\";", "L\"wide\"", "s"},
        {"c", "Arena *a = new(&scratch, Arena);", "Arena", "t"},
        {"c", "syscall(SYS_write, 1);", "SYS_write", ""},
        {"c", "CreateFileW(path);", "CreateFileW", ""},
        {"c", "int ifdef_count;", "if", ""},
        {"c", "if (x) foo(y);", "foo", ""},
        {"c", "a = b * sqrt(c);", "sqrt", ""},
        {"c", "if (a && ready()) {}", "ready", ""},
        {"c", "static int\nfoo(void)\n{\n}", "foo", "f"},
        {"c", "static Str *lookup(Env **env, Str key)", "lookup", "f"},
        {"c", "struct node *next;", "node", "t"},
        {"c", "x = s.int_value;", "int_value", ""},
        {"c", "typedef void (*fn)(char *key);", "key", ""},
        {"c++", "template<typename T> T *alloc(Arena *a);", "typename", "k"},
        {"cpp", "template<typename T> T *alloc(Arena *a);", "alloc", "f"},
        {"c++", "auto p = new Foo;", "new", "k"},
        {"c++", "return nullptr;", "nullptr", "k"},
        {"c++", "Guard g(&lock);", "g", ""},
        {"c++", "for (Guard g(lock); n;) {}", "g", ""},
        {"c++", "span<T> concat(Arena *a, span<T> s);", "concat", "f"},
        {"c++", "list<list<T>> f(Arena *a);", "f", "f"},
        {"c", "if (a > f(x)) {}", "f", ""},
        {"c", "a>>f(x);", "f", ""},
        {"c", "#define LP (\nint main(void) {}", "main", "f"},
        {"glsl", "uniform vec2 pos;\nvoid main() { }", "uniform", "k"},
        {"glsl", "uniform vec2 pos;\nvoid main() { }", "vec2", "t"},
        {"glsl", "uniform vec2 pos;\nvoid main() { }", "main", "f"},

        {"js", "s.replace(/[^a-z/]/g, '')", "/[^a-z/]/g", "s"},
        {"javascript", "if (/blue/.test(c)) {}", "/blue/", "s"},
        {"javascript", "let r = a / b / c;", "/ b /", ""},
        {"javascript", "let r = (a) / 2 / (b);", "2", "m"},
        {"js", "let s = `multi\nline ${x}`;", "`multi\nline ${x}`", "s"},
        {"js", "function foo() {}", "foo", "f"},
        {"js", "obj.default = null;", "default", ""},
        {"js", "obj.default = null;", "null", "k"},
        {"java", "@Override public String toString() {}", "@Override", "p"},
        {"java", "@Override public String toString() {}", "String", "t"},
        {"java", "class Foo {}", "Foo", "f"},
        {"java", "class A {\n  int mix(int a) {\n    return b(a);\n  }\n}", "mix", "f"},
        {"java", "class A {\n  int mix(int a) {\n    return b(a);\n  }\n}", "b", ""},
        {"java", "x = new Thread(r);", "Thread", "t"},
        {"go", "func main() {\n\tvar r []rune\n}", "main", "f"},
        {"go", "func main() {\n\tvar r []rune\n}", "rune", "t"},
        {"go", "s := `raw\\n`", "`raw\\n`", "s"},

        {"py", "@lru_cache(maxsize=32)\ndef f(n): return f\"{n}\"", "@lru_cache", "p"},
        {"py", "@lru_cache(maxsize=32)\ndef f(n): return f\"{n}\"", "f\"{n}\"", "s"},
        {"py", "@lru_cache(maxsize=32)\ndef f(n): return f\"{n}\"", "f", "f"},
        {"python", "x = b'\\x00'  # it's", "b'\\x00'", "s"},
        {"python", "x = b'\\x00'  # it's", "# it's", "c"},
        {"py", "s = '''a\n'b'\nc'''", "'''a\n'b'\nc'''", "s"},
        {"py", "nums: list[int] = []", "list", "t"},
        {"ruby", "f = lambda { i } # => 1\nx = :sym", ":sym", "v"},
        {"ruby", "f = lambda { i } # => 1\nx = :sym", "# =&gt; 1", "c"},
        {"julia", "c = 'π'  # char\ny = x'", "'π'", "s"},
        {"julia", "c = 'π'  # char\ny = x'", "# char", "c"},
        {"lua", "local function f(x) -- hi\nreturn x ~= nil end", "-- hi", "c"},
        {"lua", "local function f(x) -- hi\nreturn x ~= nil end", "f", "f"},
        {"lua", "local function f(x) -- hi\nreturn x ~= nil end", "nil", "k"},
        {"lua", "--[[ block\ncomment ]] x = 1", "--[[ block\ncomment ]]", "c"},
        {"php", "$r = bar(1)();  // $r = 1", "$r", "v"},
        {"php", "$r = bar(1)();  // $r = 1", "// $r = 1", "c"},
        {"perl", "my @a = ($x % 2); # c", "@a", "v"},
        {"perl", "my @a = ($x % 2); # c", "$x", "v"},
        {"perl", "my @a = ($x % 2); # c", "# c", "c"},
        {"perl", "print $$ref{x}, $#arr;", "$$ref", "v"},
        {"perl", "print $$ref{x}, $#arr;", "$#arr", "v"},

        {"matlab", "q = floor(p) + dirs'; % it's", "% it's", "c"},
        {"octave", "x = [1 2]';\ny = 'str';", "'str'", "s"},
        {"octave", "disp('it''s') # c", "'it''s'", "s"},
        {"octave", "disp('it''s') # c", "# c", "c"},
        {"matlab", "function img = mandel (w)\n  img = w;\nend", "mandel", "f"},
        {"matlab", "function img = mandel (w)\n  img = w;\nend", "img", ""},
        {"matlab", "function [a, b] = f(x) % a = b\n", "f", "f"},
        {"matlab", "function [a, b] = f(x) % a = b\n", "a", ""},
        {"octave", "function show(x)\n  disp(x)\nend", "show", "f"},
        {"sql", "select a from t where b = 'it''s' -- note", "select", "k"},
        {"sql", "select a from t where b = 'it''s' -- note", "'it''s'", "s"},
        {"sql", "select a from t where b = 'it''s' -- note", "-- note", "c"},
        {"sql", "CREATE TABLE t (id INTEGER)", "INTEGER", "t"},
        {"qbasic", "PRINT \"hi\" ' it's", "' it's", "c"},
        {"qbasic", "PRINT \"hi\" ' it's", "PRINT", "k"},
        {"qbasic", "x& = &H10000 MOD n", "&amp;H10000", "m"},
        {"qbasic", "FUNCTION squeeze% (r AS sponge4)", "squeeze%", "f"},
        {"qbasic", "for i = 1 to 10", "to", "k"},
        {"qbasic", "REM a \"comment\"", "REM a \"comment\"", "c"},
        {"vim", "\" comment\nmap <leader>d :call system(\"x\")<cr>", "\" comment", "c"},
        {"vim", "\" comment\nmap <leader>d :call system(\"x\")<cr>", "&lt;leader&gt;", "v"},
        {"vim", "\" comment\nmap <leader>d :call system(\"x\")<cr>", "\"x\"", "s"},
        {"vim", "set encoding=utf-8", "8", ""},
        {"gnuplot", "plot 'a.csv' using 1:2 # c", "plot", "k"},
        {"gnuplot", "plot 'a.csv' using 1:2 # c", "'a.csv'", "s"},
        {"json", "{\"a\": [1, true, \"s\"]}", "\"a\"", "v"},
        {"json", "{\"a\": [1, true, \"s\"]}", "true", "k"},
        {"json", "{\"a\": [1, true, \"s\"]}", "\"s\"", "s"},

        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "defun", "k"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "foo", "f"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "&amp;optional", "k"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "\"doc\"", "s"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "; c", "c"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "?\"", "s"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "'sym", "v"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", ":key", "v"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "#'car", "v"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "#x1F", "m"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "1.5", "m"},
        {"cl", "(defun foo (x &optional y)\n  \"doc\" ; c\n  (list ?\" ?a 'sym :key #'car #x1F 1.5))", "list", ""},
        {"elisp", "(let ((x 1)) (setq x (1+ x)))", "let", "k"},
        {"elisp", "(let ((x 1)) (setq x (1+ x)))", "1+", ""},
        {"lisp", "(let-alist x)", "let", ""},
        {"scheme", "(define (square x) (* x x)) #t #\\a", "square", "f"},
        {"scheme", "(define (square x) (* x x)) #t #\\a", "#t", "k"},
        {"scheme", "(define (square x) (* x x)) #t #\\a", "#\\a", "s"},
        {"clojure", "(defn f [x] (re-find #\"a+\" x)) ; c", "#\"a+\"", "s"},
        {"clojure", "(defn f [x] (re-find #\"a+\" x)) ; c", "f", "f"},
        {"clojure", "(identical? a b)", "?", ""},
        {"wat", "(func $f (param i32) (;c;) (i32.const 1)) ;; x", "$f", "f"},
        {"wat", "(func $f (param i32) (;c;) (i32.const 1)) ;; x", "i32", "t"},
        {"wat", "(func $f (param i32) (;c;) (i32.const 1)) ;; x", "(;c;)", "c"},
        {"wat", "(func $f (param i32) (;c;) (i32.const 1)) ;; x", "i32.const", "k"},
        {"wat", "(func $f (param i32) (;c;) (i32.const 1)) ;; x", ";; x", "c"},

        {"nasm", "x: dd 0\n.L0: mov eax, dword [rel x] ; c", "x", "f"},
        {"nasm", "x: dd 0\n.L0: mov eax, dword [rel x] ; c", "dd", "p"},
        {"nasm", "x: dd 0\n.L0: mov eax, dword [rel x] ; c", ".L0", "f"},
        {"nasm", "x: dd 0\n.L0: mov eax, dword [rel x] ; c", "mov", "k"},
        {"nasm", "x: dd 0\n.L0: mov eax, dword [rel x] ; c", "eax", "v"},
        {"nasm", "x: dd 0\n.L0: mov eax, dword [rel x] ; c", "dword", "t"},
        {"nasm", "x: dd 0\n.L0: mov eax, dword [rel x] ; c", "; c", "c"},
        {"nasm", "%define N 9\nvmovdqu64 zmm0, [r8d+N]", "%define", "p"},
        {"nasm", "%define N 9\nvmovdqu64 zmm0, [r8d+N]", "N", "f"},
        {"nasm", "%define N 9\nvmovdqu64 zmm0, [r8d+N]", "zmm0", "v"},
        {"nasm", "%define N 9\nvmovdqu64 zmm0, [r8d+N]", "r8d", "v"},
        {"nasm", "mov rdi, 1 # not a comment", "# not a comment", ""},
        {"att", "1:  cmp %rax, %rcx  // it's\n    ja 1b # c", "1", "f"},
        {"att", "1:  cmp %rax, %rcx  // it's\n    ja 1b # c", "cmp", "k"},
        {"att", "1:  cmp %rax, %rcx  // it's\n    ja 1b # c", "%rax", "v"},
        {"att", "1:  cmp %rax, %rcx  // it's\n    ja 1b # c", "// it's", "c"},
        {"att", "1:  cmp %rax, %rcx  // it's\n    ja 1b # c", "1b", "f"},
        {"att", "1:  cmp %rax, %rcx  // it's\n    ja 1b # c", "# c", "c"},
        {"att", "movl $1048616, 8(%esp); ret", "$1048616", "m"},
        {"att", "movl $1048616, 8(%esp); ret", "ret", "k"},
        {"att", "movb $'a, %al\ncmpb $'\\n', %al", "'a", "s"},
        {"att", "movb $'a, %al\ncmpb $'\\n', %al", "%al", "v"},
        {"att", "movb $'a, %al\ncmpb $'\\n', %al", "'\\n'", "s"},
        {"att", ".globl f\nf: ret", ".globl", "p"},
        {"att", ".globl f\nf: ret", "f", "f"},
        {"aarch64", "_start: ldr w0, [sp] ; argc", "_start", "f"},
        {"aarch64", "_start: ldr w0, [sp] ; argc", "ldr", "k"},
        {"aarch64", "_start: ldr w0, [sp] ; argc", "w0", "v"},
        {"aarch64", "_start: ldr w0, [sp] ; argc", "sp", "v"},
        {"aarch64", "_start: ldr w0, [sp] ; argc", "; argc", "c"},

        {"sh", "#!/bin/sh\nexport PATH=\"$HOME/bin:$PATH\" # c", "#!/bin/sh", "c"},
        {"sh", "#!/bin/sh\nexport PATH=\"$HOME/bin:$PATH\" # c", "export", "k"},
        {"sh", "#!/bin/sh\nexport PATH=\"$HOME/bin:$PATH\" # c", "PATH", "v"},
        {"sh", "#!/bin/sh\nexport PATH=\"$HOME/bin:$PATH\" # c", "$HOME", "v"},
        {"sh", "#!/bin/sh\nexport PATH=\"$HOME/bin:$PATH\" # c", "# c", "c"},
        {"bash", "echo a#b $# 'it'", "a#b", ""},
        {"bash", "echo a#b $# 'it'", "$#", "v"},
        {"bash", "echo a#b $# 'it'", "'it'", "s"},
        {"sh", "x=\"$(cmd \"$y\")\"; done", "$(", "v"},
        {"sh", "x=\"$(cmd \"$y\")\"; done", "$y", "v"},
        {"sh", "x=\"$(cmd \"$y\")\"; done", "done", "k"},
        {"sh", "for x in a b; do rm x-done; done", "in", "k"},
        {"sh", "for x in a b; do rm x-done; done", "x-done", ""},
        {"make", ".POSIX:\nCC = cc\nall: main.o\n\t$(CC) -o $@ $^ # c\n", ".POSIX", "p"},
        {"make", ".POSIX:\nCC = cc\nall: main.o\n\t$(CC) -o $@ $^ # c\n", "CC", "v"},
        {"make", ".POSIX:\nCC = cc\nall: main.o\n\t$(CC) -o $@ $^ # c\n", "all", "f"},
        {"make", ".POSIX:\nCC = cc\nall: main.o\n\t$(CC) -o $@ $^ # c\n", "$(CC)", "v"},
        {"make", ".POSIX:\nCC = cc\nall: main.o\n\t$(CC) -o $@ $^ # c\n", "$@", "v"},
        {"make", ".POSIX:\nCC = cc\nall: main.o\n\t$(CC) -o $@ $^ # c\n", "# c", "c"},
        {"makefile", "x.o: x.c\n    $(CC) -c x.c\n", "$(CC)", "v"},
        {"makefile", "LIBS += -lm\n", "LIBS", "v"},
        {"make", "foo-$(V).tar: $(EL)\n", "foo-", "f"},
        {"make", "foo-$(V).tar: $(EL)\n", "$(V)", "v"},
        {"make", "foo-$(V).tar: $(EL)\n", ".tar", "f"},
        {"diff", "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n ctx\n", "--- a/x", "gh"},
        {"diff", "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n ctx\n", "+++ b/x", "gh"},
        {"diff", "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n ctx\n", "@@ -1 +1 @@", "p"},
        {"diff", "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n ctx\n", "-old", "gd"},
        {"diff", "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n ctx\n", "+new", "gi"},
        {"udiff", "--- removed comment\n", "--- removed comment", "gd"},

        {"xml", "<?xml version=\"1.0\"?>\n<a href=\"x\">t &amp; u<!-- c --></a>", "&lt;?xml version=\"1.0\"?&gt;", "p"},
        {"xml", "<?xml version=\"1.0\"?>\n<a href=\"x\">t &amp; u<!-- c --></a>", "&lt;a", "k"},
        {"xml", "<?xml version=\"1.0\"?>\n<a href=\"x\">t &amp; u<!-- c --></a>", "href", "v"},
        {"xml", "<?xml version=\"1.0\"?>\n<a href=\"x\">t &amp; u<!-- c --></a>", "\"x\"", "s"},
        {"xml", "<?xml version=\"1.0\"?>\n<a href=\"x\">t &amp; u<!-- c --></a>", "&amp;amp;", "v"},
        {"xml", "<?xml version=\"1.0\"?>\n<a href=\"x\">t &amp; u<!-- c --></a>", "&lt;!-- c --&gt;", "c"},
        {"xml", "<?xml version=\"1.0\"?>\n<a href=\"x\">t &amp; u<!-- c --></a>", "&lt;/a", "k"},
        {"html", "<!DOCTYPE html>\n<br/>", "&lt;!DOCTYPE html&gt;", "p"},
        {"html", "<!DOCTYPE html>\n<br/>", "/&gt;", "k"},
        {"yaml", "---\nkey: \"v\" # c\nlist: [a, 2]\ntext: >-\n  folded\n  lines\nnext: true\n", "---", "p"},
        {"yaml", "---\nkey: \"v\" # c\nlist: [a, 2]\ntext: >-\n  folded\n  lines\nnext: true\n", "key", "v"},
        {"yaml", "---\nkey: \"v\" # c\nlist: [a, 2]\ntext: >-\n  folded\n  lines\nnext: true\n", "\"v\"", "s"},
        {"yaml", "---\nkey: \"v\" # c\nlist: [a, 2]\ntext: >-\n  folded\n  lines\nnext: true\n", "# c", "c"},
        {"yaml", "---\nkey: \"v\" # c\nlist: [a, 2]\ntext: >-\n  folded\n  lines\nnext: true\n", "2", "m"},
        {"yaml", "---\nkey: \"v\" # c\nlist: [a, 2]\ntext: >-\n  folded\n  lines\nnext: true\n", "&gt;-", "p"},
        {"yaml", "---\nkey: \"v\" # c\nlist: [a, 2]\ntext: >-\n  folded\n  lines\nnext: true\n", "folded", "s"},
        {"yaml", "---\nkey: \"v\" # c\nlist: [a, 2]\ntext: >-\n  folded\n  lines\nnext: true\n", "next", "v"},
        {"yaml", "---\nkey: \"v\" # c\nlist: [a, 2]\ntext: >-\n  folded\n  lines\nnext: true\n", "true", "k"},
        {"yaml", "color: \"#ecffdc\"\n", "\"#ecffdc\"", "s"},
        {"ini", "[core]\nname = value\n; x\n", "[core]", "k"},
        {"ini", "[core]\nname = value\n; x\n", "name", "v"},
        {"ini", "[core]\nname = value\n; x\n", "; x", "c"},
        {"pc", "prefix=/usr\nCflags: -I${prefix}/include", "prefix", "v"},
        {"pc", "prefix=/usr\nCflags: -I${prefix}/include", "Cflags", "k"},
        {"pc", "prefix=/usr\nCflags: -I${prefix}/include", "${prefix}", "v"},
        {"obj", "# cube\nv -1.0 +1.0 0\nf 1//2 3//4", "# cube", "c"},
        {"obj", "# cube\nv -1.0 +1.0 0\nf 1//2 3//4", "v", "k"},
        {"obj", "# cube\nv -1.0 +1.0 0\nf 1//2 3//4", "-1.0", "m"},
        {"obj", "# cube\nv -1.0 +1.0 0\nf 1//2 3//4", "+1.0", "m"},
        {"obj", "# cube\nv -1.0 +1.0 0\nf 1//2 3//4", "4", "m"},
        {"bat", "@echo off\nrem comment\ncc %* %1 %PATH%", "echo", "k"},
        {"bat", "@echo off\nrem comment\ncc %* %1 %PATH%", "rem comment", "c"},
        {"bat", "@echo off\nrem comment\ncc %* %1 %PATH%", "%*", "v"},
        {"bat", "@echo off\nrem comment\ncc %* %1 %PATH%", "%1", "v"},
        {"bat", "@echo off\nrem comment\ncc %* %1 %PATH%", "%PATH%", "v"},
    };
    for (iz i = 0; i < countof(cases); i++) {
        Arena a    = perm;
        Str   html = hlrender(&a, cases[i].lang, cases[i].code, scratch);
        Str   name = concat(&a, concat(&a, cases[i].lang, ": "), cases[i].tok);
        if (!expect(t, name, hlclassof(html, cases[i].tok), cases[i].cls)) {
            fprintf(stderr, "  in:   [");
            printstr(stderr, html);
            fprintf(stderr, "]\n");
        }
    }

    // Exact output, unknown languages, and no language
    {
        Arena a     = perm;
        b32   known = 0;
        Str   html  = hlrender(&a, "c", "a<b && c>d", scratch, &known);
        expect(t, "hl.escape", html, "a&lt;b &amp;&amp; c&gt;d");
        expect(t, "hl.known", known ? Str("yes") : Str("no"), "yes");
        html = hlrender(&a, "c", "x = 'a'; // <b>", scratch);
        expect(t, "hl.exact", html,
               "x = <span class=\"s\">'a'</span>; "
               "<span class=\"c\">// &lt;b&gt;</span>");
        html = hlrender(&a, "bf", "<+>", scratch, &known);
        expect(t, "hl.unknown", html, "&lt;+&gt;");
        expect(t, "hl.unknown.ret", known ? Str("yes") : Str("no"), "no");
        html = hlrender(&a, "", "<+>", scratch, &known);
        expect(t, "hl.nolang", html, "&lt;+&gt;");
        expect(t, "hl.nolang.ret", known ? Str("yes") : Str("no"), "yes");
        html = hlrender(&a, "sh", "x=\"$(cmd \"$y\")\"", scratch);
        expect(t, "hl.nested", html,
               "<span class=\"v\">x</span>=<span class=\"s\">\""
               "<span class=\"v\">$(</span>cmd <span class=\"s\">\""
               "<span class=\"v\">$y</span>\"</span><span class=\"v\">)</span>"
               "\"</span>");
        html = hlrender(&a, "c++", "template<typename T>\nT *f(T x);", scratch);
        expect(t, "hl.known", hlcount(html, "<span class=\"t\">T</span>") == 3 ? Str("ok") : html, "ok");
        html = hlrender(&a, "c", "typedef long size;\nsize n;", scratch);
        expect(t, "hl.typedef", hlcount(html, "<span class=\"t\">size</span>") == 2 ? Str("ok") : html, "ok");
    }

    // Every tag used by the blog is known
    for (iz i = 0; i < countof(hltags); i++) {
        Arena a     = perm;
        b32   known = 0;
        hlrender(&a, hltags[i], "x", scratch, &known);
        expect(t, hltags[i], known ? Str("known") : Str("unknown"), "known");
    }

    // Word tables must be sorted for bisection
    {
        Slice<HlWords> tables = {};
        Arena a = perm;
        for (iz i = 0; i < countof(hltags); i++) {
            HlLang l = hllang(hltags[i]);
            tables = push(&a, tables, l.keywords);
            tables = push(&a, tables, l.types);
            tables = push(&a, tables, l.defs);
            tables = push(&a, tables, l.tdefs);
            tables = push(&a, tables, l.prefixes);
        }
        HlWords special[] = {
            HLWORDS(valuekeywords), HLWORDS(lispheads), HLWORDS(lispdefs),
            HLWORDS(watdefs), HLWORDS(wattypes), HLWORDS(nasmdirectives),
            HLWORDS(nasmsizes), HLWORDS(asmprefixes), HLWORDS(x86regs),
            HLWORDS(a64regs), HLWORDS(shkeywords), HLWORDS(makedirectives),
            HLWORDS(batkeywords), HLWORDS(yamlkeywords), HLWORDS(basicrem),
        };
        for (iz i = 0; i < countof(special); i++) {
            tables = push(&a, tables, special[i]);
        }
        Str bad = {};
        for (iz i = 0; i < tables.len; i++) {
            HlWords w = tables[i];
            for (iz j = 1; j < w.len; j++) {
                if (compare(w.data[j-1], w.data[j]) >= 0) bad = w.data[j];
            }
        }
        expect(t, "hl.sorted", bad, "");
    }

    // Arbitrary input never loses text or runs away
    {
        static u8 alphabet[] =
            "\"'`/\\*#;%$@(){}[]<>&:=.-+!?,|~^ \t\n\n0x1aeZ_Lr\xce\xbb";
        u64 rng  = 1;
        Str fail = {};
        for (i32 n = 0; n < 300 && !fail.len; n++) {
            u8 buf[64];
            rng = rng*0x3243f6a8885a308d + 1;
            iz len = (iz)(rng >> 58);
            for (iz k = 0; k < len; k++) {
                rng = rng*0x3243f6a8885a308d + 1;
                buf[k] = alphabet[(rng >> 33) % (countof(alphabet) - 1)];
            }
            Str code = {buf, len};
            for (iz i = 0; i < countof(hltags); i++) {
                Arena a    = perm;
                Str   html = hlrender(&a, hltags[i], code, scratch);
                if (!hlroundtrip(html, code)) {
                    fail = concat(&perm, concat(&perm, hltags[i], ": "), code);
                    break;
                }
            }
        }
        expect(t, "hl.fuzz", fail, "");
    }
}
