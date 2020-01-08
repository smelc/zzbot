% zzbot's documentation

# Introduction

zzbot is a command line executing tool. It takes as input a list of builders, a builder being a list of shell commands; and execute them.

# Input format

zzbot is configured by a list of builders that are specified by XML files. The syntax of XML files is as follows:

* The top-level element is `<config>`. It has no attributes.

The allowed elements at depth 1 (i.e. below `<config>`) are:

* `<builder>` which can be specified one or more times. A builder specifies a list of shell commands to execute.
* `<substitution>` which can be specified zero or one time. A substitution defines constants that can be used in the rest of the file.

The syntax of these elements is detailed in the following subsections. One should also refer to the [variables](#variables) subsection as variables play an important role in the flexibility of zzbot's input format.

## `<builder>`

Element `<builder>` specifies a list of shell commands to execute. Its attribute are:

* The mandatory `<name>` attribute which is used for display and indexing. All builders of a file must specify a different name.
* The optional `<workdir>` attribute which specifies the working directory of all commands of this builder. If omitted it defaults to the path from where zzbot is launched.

`<builder>` accepts the following children:

* `<shell>` whose attributes are:
    * The mandatory `command` attribute which specifies the shell command to execute.
    * The optional `workdir` attribute which specifies the working directory where to execute the command. If omitted, it defaults to the working directory of the enclosing builder.
    * The optional Boolean `haltOnFailure` attribute which specifies whether the enclosing builder's execution must stop if the command fails. If omitted, it defaults to `"True"`.
* `<setPropertyFromCommand>` which has the same attributes as `<shell>` with the following changes:
    * The addition of the mandatory `property` attribute. This attribute specifies the name of the build property to which the standard output of the command is assigned to.
    * The default value of `haltOnFailure` is `"False"`. The rationale is that a common idiom is to do something if a file is missing, in which case `<setPropertyFromCommand>` calls command `ls`, which will fail while still being a valid build execution.
* `<setProperty>` whose syntax is `<setProperty property="foo" value="bar">` which assigns the value `bar` to the build property `foo`. As the value may contain variables, `<setProperty>` can be used to build variables from constant strings or from the concatenation of constants and other variables.

## `<substitution>` {#substitution}

Element `<substitution>` accepts zero or many `<entry>` children whose syntax is `<entry name="foo" value="bar">`. Such an entry defines the constant `foo` and assigns the value `bar` to it. Constants can be used anywhere in the rest of the file with the syntax `$[foo]` which will get substituted by the value `bar`.

## Variables {#variables}

Input files may feature three kind of variables:

* Variables bound by [`<substitution>`](#substitution) which are used with the `$[foo]` syntax where `foo` is the name of the variable. Such variables are used to avoid repetitions in input files and to gather all installation-dependent definitions in the same place.
* Environment variables which are used with the `${foo}` syntax (curly braces ARE mandatory).
* Build variables which are used with the `«foo»` syntax. Such variables are used to condition and configure the build according to both other variables (with `<setProperty>`) and the output of commands (with `<setPropertyFromCommand>`).
