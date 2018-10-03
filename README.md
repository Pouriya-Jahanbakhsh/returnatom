# `ReturnAtom`
Erlang library for creating dynamic modules that their functions return `atom`.

# Example
```sh
~/returnatom $ make shell
===> Verifying dependencies...
===> Compiling returnatom
```
```erlang
(returnatom@localhost)1> returnatom:start().
ok
(returnatom@localhost)2> returnatom:start(foo).
ok
(returnatom@localhost)3> returnatom:add(foo, bar, baz).
ok
(returnatom@localhost)4> foo:bar().
baz
(returnatom@localhost)5> returnatom:replace(foo, bar, qux).
ok
(returnatom@localhost)6> foo:bar().                        
qux
(returnatom@localhost)7> returnatom:delete(foo, bar).      
ok
(returnatom@localhost)8> returnatom:modules().       
[foo]
```
