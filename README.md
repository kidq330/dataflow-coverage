# frama-c-plugin-cfg-view
The control flow graph visualisation plugin.

```bash
frama-c test.c -load-script cfg_print.ml -eva -then -cfg
dot -Tpng cfg.dot > cfg.png
open cfg.png
```

![Control flow graph](cfg.png "Control flow graph")
