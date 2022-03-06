# frama-c-plugin-cfg-view
The control flow graph visualisation plugin.

```bash
frama-c -load-script cfg_print.ml test.c
dot -Tpng cfg.out > cfg.png
open cfg.png
```

![Control flow graph](cfg.png "Control flow graph")
