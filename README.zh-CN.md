### Trekker
Trekker 是一个基于 PyQt 的 Emacs 浏览器， 核心代码来源于 [EAF](https://github.com/emacs-eaf/emacs-application-framework/), Trekker 的设计目标如下：
1. 全功能浏览器： 支持最新的浏览器技术， 而不仅仅只是纯文本浏览器
2. 高性能： 基于多进程和多线程技术开发， 多线程保证不会卡住 Emacs， 多进程可以充分利用多核性能， 避免打开太多网页后， 网页渲染速度降低， 这也是和 eaf-browser 最主要的区别
3. 可扩展性： 可以通过 Qt、 Python 和 JavaScript 三种技术来扩展浏览器的功能
4. 插件体系： 用户可以选择加载的 JS 插件， 来自由的平衡速度和功能

### 安装
1. 安装 Python 依赖: epc, sexpdata, six: `pip3 install epc sexpdata six`
2. 用 `git clone` 下载此仓库， 并替换下面配置中的 load-path 路径
3. 把下面代码加入到你的配置文件 ~/.emacs 中：

```elisp
(add-to-list 'load-path "<path-to-trekker>")

(require 'trekker)
(trekker-enable)
```

### 为什么叫 Trekker 这个名字？
在很多 Emacser 眼中我是异教徒， 很多 Emacser 追求 "All in Emacs", 但是却不切实际的要求所有代码都需要用 Elisp 来实现， 当我实现了 EAF 和 lsp-bridge 以后， 我已经实现了 "All in Emacs", 而且我认为多语言扩展 Emacs 是一种非常务实， 而且未来无限的道路， 因为它不需要修改 Emacs 代码就可以实现多线程编程和多媒体功能。

这条路我已经走了很多年， 但是参与开发的 Emacser 人数依然不多， 所以， 我把新的浏览器取名叫作 Trekker, 寓意旅行者、 徒步者， 开拓更多的可能性。

### 为什么用 PyQt 去实现？
1. 首先， 用 Qt 可以很好的在 Linux X11 和 Windows 平台实现跨进程嵌入和多实例镜像， 体验更好一点
2. 其二， Qt 现代语言绑定中， 只有 Python 绑定比较完备， Rust 和 Golang 等性能更高的语言， 绑定都非常不成熟 
3. 最后， 目前只有 Chromium 内核的引擎比较快， 有 CEF 和 QtWebEngine 两种方式， 而 QtWebEngine 这种方式和 Qt 结合更好一点



