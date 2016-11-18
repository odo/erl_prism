# erl_prism
like htop for erlang nodes

![erl_prism](../master/priv/badge.png?raw=true "erl_prism")

## warning
this software is experimental, do not point it at your production system!

## installation

```
git clone git@github.com:odo/erl_prism.git
cd erl_prism
./rebar get com
```

## usage

make sure you set `export TERM=xterm-256color`

### starting

```
./erl_prism
usage:
	erl_prism host cookie [Options]
options:
	-a [Integer]: auto-capture every n seconds
```
### keys

```
'1' or 'r'       : reductions mode
'2' or 'm'       : memory mode
'3' or 'q'       : message queue mode
arrows up/down   : move the cursor
arrows left/right: move between captures
space            : expand/collaps pool
return           : capture again
x                : export selected item as CSV
```

## example

`./erl_prism some_node@foobar.local zecookie`

![screenshot](../master/priv/panel.png?raw=true "screenshot")

## explanation

### tree

what you see is the supervision tree of the node. The lables are:

```
n: node
a: application
s: supervisor
w: worker
p: pool
```

### modes

erl_prism has 3 modes: reductions, memory and message queue length.

### bars

The bars and numbers at the right correspond to the mode. Nodes, applications and supervisors show the accumulated value, worker show their own value.

Bars might be stacked showing the absolute value in bright colors and the value relative to its supervisor in dimmed colors.

### pools

`pools` are collections of workers that were started using the same initial call.
The balance value is the reverse [Gini index](https://www.wikiwand.com/en/Gini_coefficient) of the values corresponding to the current mode.
You can expand pools them by pressing the space bar.

