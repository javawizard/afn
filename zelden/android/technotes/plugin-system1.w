
Working on a plugin system here...

I think Zelden should be written similarly to how Eclipse is written: a single plugin manager application bootstraps everything and then loads a single plugin, which, in turn, loads others.

For now, I'm going to just have it so that plugins are loaded when Zelden starts up. This will make a lot of client-server interaction stuff easier.
