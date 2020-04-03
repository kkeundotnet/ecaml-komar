ECaml
===

A simple template engine for OCaml

This is a fork of ECaml of Alexander Markov.  Visit http://komar.in/en/code/ecaml to get the
original version.

Build
---

```
$ make
```

Run tests
---

```
$ make test
```

Install
---

```
$ opam pin add ecaml-komar https://github.com/kkeundotnet/ecaml-komar.git
```

or

```
$ make install
$ make uninstall
```

Usage
---

Note that the binary and package name is `ecaml-komar`, not `ecaml`, because there is a package that
have the same name already, [`Ecaml` package](https://github.com/janestreet/ecaml) by Jane Street.

```
$ ecaml-komar --help
ECaml - A simple template tool for OCaml
Usage: ecaml-komar [OPTIONS...] <.eml file>

  -o FILE	destination file to output an OCaml code; default is sourcename.ml
  -p STR	printer function to apply to strings; default is print_string
  -esc-p STR	function to apply to <%= %> parts but not to <%=raw %>; default is the same as -p
  -d 		write a directive with original file name for more impressive error messages
  -header STR	header to write before the output
  -footer STR	footer to write after the output
  -help  Display this list of options
  --help  Display this list of options
```

Template syntax
---

* `<% %>` are for code.
* `%` in the beginning of line is for code too.
* `<%= %>` are for "print this expression"; if `-esc-p` is set — also escape string (useful for
  HTML).
* `<%=raw %>` are also for "print this expression", but do not escape anything inside even if
  `-esc-p` is set.

Alexander Markov said:

> This tool is so stupid, so don't forget about ; at the end of your expressions!
>
> I don't want to add any other features yet, because I want to keep this tool simple.
