rebar_vsn_plugin
================

NOTE
----

The rebar vsn plugin has a new feature where it can calculate a good
version for an Erlang app based solely on the *.app.src file. You
no longer need to place `semver` in your version field unless you
simply want to continue the old behaviour. The old behaviour is still
fully supported.

TLDR
----

This plugin will make accurate [semver](http://semver.org) compatible
version strings for your Erlang OTP Apps as long as you are doing
semver style versioning with tags 'v<version>'.

Use
---

Add the following dep specification to the deps tuple of your
`rebar.config`:

    {rebar_vsn_plugin, "",
         {git, "https://github.com/erlware/rebar_vsn_plugin.git",
          {branch, "master"}}},

Then inform rebar that you want this to be used as a plugin like so:

    {plugins, [rebar_vsn_plugin]}.

In Rebar, to make sure that the plugin is compiled and loaded before
the actual Rebar is started, you should add the following to your
Rebar config:

    {plugin_dir, "deps/rebar_vsn_plugin/src"}.

Then add the semver version to the `<app-name>.app.src` file. It
should go from something like:

    {application, rebar_vsn_plugin,
       [{description, "Correct version manipulation for rebar"},
        {vsn, git},
        {modules, []},
        {registered, []},
        {applications, [kernel, stdlib]}]}.

to this the actual version you are interested in using.

    {application, rebar_vsn_plugin,
       [{description, "Correct version manipulation for rebar"},
        {vsn, "0.0.5"},
        {modules, []},
        {registered, []},
        {applications, [kernel, stdlib]}]}.

The key change is having the version you wish to use `{vsn, "0.0.5"}`
in the version field.

If you wish to maintain the original 'tag oriented' behaviour you can
replace `{vsn, git}` with `{vsn, "semver"}`. This will give you the
same behaviour as the git approach, but with full semver versions.

So your app file would look as follows:

    {application, rebar_vsn_plugin,
       [{description, "Correct version manipulation for rebar"},
        {vsn, "semver"},
        {modules, []},
        {registered, []},
        {applications, [kernel, stdlib]}]}.


Explanation
-------------

This plugin is designed to take the latest semver
compatible tag and turn it into a semver compatible version for the
OTP Application. One of the key things it does (aside from making sure
that semver is respected) is insure that there is a unique
monotonically increasing version for each commit built. It does this
by creating a version from both the latest tag, the epoch timestamp and
the ref. The ref is actually only there to make the version human
readable.

So lets say you have a repository with the tag `v0.0.1` and the epoch
`1348518514` on the latest commit identified by `26ff3c6` then you
would end up with the version `0.0.1+build.1348518514.26ff3c6`. While
that version string is long, it is perfectly accurate, semver
compatible, and works well with OTP. This solves many of the current
versioning problems with rebar and erlang OTP Apps.
