### Trekker
Trekker 是一个基于 PyQt 的 Emacs 浏览器， 核心代码来源于 [EAF](https://github.com/emacs-eaf/emacs-application-framework/), Trekker 的设计目标如下：
1. 全功能浏览器： 支持最新的浏览器技术， 而不仅仅只是纯文本浏览器
2. 高性能： 基于多进程和多线程技术开发， 多线程保证不会卡住 Emacs， 多进程可以充分利用多核性能， 避免打开太多网页后， 网页渲染速度降低， 这也是和 eaf-browser 最主要的区别
3. 可扩展性： 可以通过 Qt、 Python 和 JavaScript 三种技术来扩展浏览器的功能, 支持油猴脚本

### 安装
1. 安装 Python 依赖: epc, sexpdata, six: `pip3 install epc sexpdata six`
2. 用 `git clone` 下载此仓库， 并替换下面配置中的 load-path 路径
3. 把下面代码加入到你的配置文件 ~/.emacs 中：

```elisp
(add-to-list 'load-path "<path-to-trekker>")

(require 'trekker)
(trekker-enable)
```

### 为什么用 PyQt 去实现？
PyQt 兼顾多进程嵌入、 Qt 绑定完整、 Chromium 引擎渲染三方面的技术， 其他图形库或者其他语言绑定都很难达到 PyQt 的成熟度。

### 任务
- PyQt 和 Emaccs 之间的互调用： 已实现
- 浏览器多进程框架， 标签进程之间通讯： 已实现
- Emacs Buffer/View 在 PyQt 上的实现
- 移植 EAF Browser 功能， 包括 Cookie， 主题切换， Vimium
- 支持油猴插件框架
- 开发一件安装脚本
