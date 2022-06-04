#lang scribble/manual

@(require (for-label pollen/setup racket) "mb-tools.rkt" pollen/private/version)

@title[#:tag "first-tutorial"]{第一个教程：项目服务器和预处理器}

在本教程中，您将使用 Pollen 制作带有诗歌的单个 HTML 页面。您将了解：

@itemlist[

@item{ Pollen 与 Racket 的关系}

@item{DrRacket，代码编辑器}

@item{项目服务器}

@item{预处理器}

]

如果您想以最短的时间了解 Pollen ，请尝试@secref["quick-tour"]。

@section[#:tag-prefix "tutorial-1"]{先决条件}

我假设你已经 @seclink["Installation"]{安装}了 Racket 和 Pollen。如果没有，现在就去做。

我还将假设您了解使用命令行运行程序和使用 @exec{cd} 和 @exec{ls} 等命令导航文件系统的基础知识。 

@section{选读： Racket 与 Pollen 的关系}

正如我在@secref["big-picture"] 中提到的，Pollen 是使用 Racket 构建的，Pollen 中的所有内容最终都变成了 Racket 代码。如果你对这个想法感到满意，你可以继续往下读。

但如果不是，或者如果你只是一个好奇的人：

Racket 作为一种编程语言的关键特性之一是它提供了创建 @italic{其他} 编程语言的工具。这些语言的外观和行为可能@link["http://docs.racket-lang.org/ts-guide/index.html"]{类似于 Racket}。他们也@link["http://hashcollision.org/brainfudge/index.html"]{可能不会}。这些语言可能服务于通用目的，但更多时候它们专门用于特定目的，在这种情况下，它们被称为@italic{领域特定语言}或@italic{DSLs}。

如果您觉得这是一个奇怪的想法，那么您并不孤单。大多数程序员——直到最近，我也是——从未制作或使用过 DSL。如果你有一个编程问题要解决，你可以从 Python、Java 或 Ruby 等通用语言开始，然后从那里开始。这并没有错。

但是编程语言包含它们自己的设计选择和妥协。有时，手头的问题最好通过在更高层次上操纵编程语言来解决。当您制作 DSL 时，您仍在使用底层语言进行编程，但这样做的效率更高。

Pollen 是在 Racket 中实现的 DSL。它是 @other-doc['(lib "scribblings/scribble/scribble.scrbl")] 的近亲，另一个 Racket DSL 是为编写 Racket 文档而设计的。 Scribble 以及 Pollen 的主要特点是它是基于文本的。这意味着，虽然大多数语言的源文件都是由嵌入了文本的代码组成的，但 Pollen 的源文件是嵌入了代码的文本。

此外，Pollen 意味着距离 Racket 仅一步之遥——您可以将其视为更方便的 Racket 代码表示法系统，类似于 Markdown 是一种更方便的 HTML 表示法。但不像 Markdown，它只允许你访问 HTML 的一个子集，任何可以在 Racket 中完成的事情也可以在 Pollen 中完成。 （与 Markdown 不同的是，Pollen 和 Racket 是完整的编程语言。）

当您使用 Pollen 时，您将熟悉 Pollen 如何使用 @seclink["pollen-command-syntax"]{corresponds to Racket}，并且能够轻松地将命令从一种表示法转换为另一种表示法。在后面的教程中，您将看到如何从 Pollen 和 Racket 源文件的混合中组装出更大的 Pollen 项目。

但是在较小的项目中，比如这个，你可以只使用 Pollen。

@section{在 DrRacket 中创建一个新文件}

DrRacket 是 Racket 编程语言和其他用 Racket 制作的语言（如 Pollen）的 IDE。 IDE 代表“集成开发环境”，这是一个花哨的短语，表示“编辑和运行代码的好地方”。 DrRacket 作为核心 Racket 发行版的一部分安装。

@margin-note{有经验的程序员可能更喜欢使用通用文本编辑器和命令行来编辑和运行 Racket 程序。没关系。但是对于这些教程，我假设您使用的是 DrRacket。否则，我只能相信你自己会弄清楚，你需要做什么才能跟上。}

启动 DrRacket。开始一个新文件。文件中的代码将如下所示：

@codeblock{
#lang racket
}

在主窗口中，您还应该看到一个 @defterm{交互窗口}，它显示了当前文件的输出，并开始看起来像这样（细节，如版本号，会有所不同）：

@terminal{
Welcome to DrRacket, version @(version)--2015-11-26(-/f) [3m].
Language: racket; memory limit: 1000 MB.
> }

如果您没有看到交互窗口，请从菜单中选择 @menuitem["View" "Show Interactions"]。

@subsection{设置@tt{#lang} 行}

每个 Racket 源文件和每个 Pollen 源文件的第一行称为 @defterm{@tt{#lang} 行}。 @tt{#lang} 行标识用于解释文件其余部分的语言。 

@margin-note{有关@tt{#lang} 行的更多信息，请参阅@secref["hash-languages" #:doc '(lib "scribblings/guide/guide.scrbl")]。顺便说一句，@tt{#lang} 发音为@italic{hash-lang}。}

当您在 DrRacket 中启动新的 Pollen 源文件时，您需要将 @tt{#lang} 行更改为 Pollen 语言。最简单的方法是将第一行更改为：

@codeblock{
#lang pollen
}

现在通过单击右上角的@onscreen["Run"] 按钮运行您的文件，或者从菜单中选择@menuitem["Racket" "Run"]。你会得到类似的东西：

@terminal{
Welcome to DrRacket, version @(version)--2015-11-26(-/f) [3m].
Language: pollen; memory limit: 1000 MB.
> 
}

请注意，语言现在报告为 @code{pollen} 。如果你愿意，请将 @tt{#lang} 行更改为：

@nested[#:style 'code-inset]{@verbatim{
#lang pollenxyz}}

然后再次单击@onscreen["Run"]。 DrRacket 将打印一个错误：

@errorblock{Module Language: invalid module text
standard-module-name-resolver: collection not found ...}

为什么？因为没有称为@code{pollenxyz} 的语言。将其切换回@code{pollen}，让我们继续。

@subsection{把诗的正文放进去}

这是我写的一首关于 CSS 的短小而糟糕的诗。

@terminal{
The margin is 42em.
The border is red.
The padding is 15em.
The border is too.
}

将这首诗的文本粘贴到您的 DrRacket 编辑窗口中，@tt{#lang} 行下方，如下所示：

@codeblock{
#lang pollen

The margin is 42em.
The border is red.
The padding is 15em.
The border is too.}

再一次 @onscreen["Run"] 文件。在交互窗口中，您将看到：

@repl-output{
The margin is 42em.
The border is red.
The padding is 15em.
The border is too.}

这向您展示了一些重要的东西：默认情况下， Pollen 源文件中的任何纯文本都只是在您 @onscreen["Run"] 文件时打印（减去 @tt{#lang} 行，这只是为了 Racket 的好处）。如果您愿意，可以编辑这首诗的文本并再次单击@onscreen["Run"]。您将在交互窗口中看到打印的更新文本。

@subsection{保存和命名您的源文件}

@(noskip-note)

Pollen 中的文件命名是必然的。

最终，您项目中的每个 Pollen 源文件都将被 @seclink["Render"]{@defterm{渲染}} 转换为输出文件。每个 Pollen 源文件对应一个输出文件。 @bold{此输出文件的名称将是源文件的名称减去 Pollen 源扩展名。}因此名为@filepath{file.txt.pp}的源文件将变为@filepath{file.txt}； @filepath{index.html.pp} 将变为 @filepath{index.html}。

因此，为了导出源文件的名称，我们

@itemlist[#:style 'ordered
@item{T为输出文件取我们想要的名称，然后}
@item{附加适当的 Pollen 文件扩展名。}
]

不同的 Pollen 源文件使用不同的扩展名——稍后会详细介绍。目前，您将用于源代码的扩展名是@filepath{.pp}。

在这种情况下，假设我们希望得到一个名为@filepath{poem.html} 的文件。因此，我们的源文件的名称需要是：

输出名称 @filepath{poem.html} 
@(linebreak)+ 源扩展名 @filepath{.pp} 
@(linebreak)= @filepath{poem.html.pp} 

在本教程的其余部分，如果你想把文件命名为@filepath{something-else.html.pp}，请自便。源文件的前缀没有任何特殊含义。只有后缀有。

@margin-note{如果您的系统或文本编辑器让您对有两个文件扩展名感到不满，您也可以使用下划线字符 (@litchar{_}) 作为内部扩展名。因此，您的源文件将被命名为@filepath{poem_html.pp}，而不是@filepath{poem.html.pp}。这个文件名的工作方式完全相同，在渲染时仍然会产生@filepath{poem.html}。}

在方便的位置（例如，您的主目录或桌面）为您的项目创建一个名为 @code{tutorial} 的新目录。在这个新目录中，将 DrRacket 文件保存为 @filepath{poem.html.pp}。

@fileblock["/path/to/tutorial/poem.html.pp" @codeblock{
#lang pollen

The margin is 42em.
The border is red.
The padding is 15em.
The border is too.
}]


@section{使用项目服务器}

项目服务器是 Pollen 中内置的 Web 服务器。正如 DrRacket 允许您运行单个文件并查看它们是否按预期工作一样，项目服务器允许您通过 Web 浏览器预览和测试您的项目。在处理 Pollen 项目时，您可能会发现在一半屏幕上打开 DrRacket 很方便，在另一半屏幕上打开指向项目服务器的 Web 浏览器。

@image/rp["project-server.png" #:scale 0.7]

``为什么我不能直接在浏览器中打开 HTML 文件？'' 如果您想继续像 1996 年那样制作网页，请继续。但这种方法有几个缺点。首先，当您直接在浏览器中打开文件时，您访问的是本地文件系统，而绝对 URL（以 @litchar{/} 开头的那种）将不起作用。其次，您必须提前渲染 HTML 文件，而项目服务器很聪明地会动态地执行此操作。第三，如果您想在自己机器以外的设备上测试您的网站——抱歉，您不能。

所以使用项目服务器。

@margin-note{即使您不打算使用 Pollen 制作网页，Web 浏览器也可以显示所有常见的数字文件，因此项目服务器最终仍然是一个方便的预览工具。}

关于安全的说明。项目服务器不是为实际使用而设计的，而是作为一种开发工具。也就是说，一旦您启动项目服务器，它就是在您的计算机上运行的实际 Web 服务器，它将响应来自任何计算机的请求。如果您想限制本地网络或本地网络上某些机器的流量，那么相应地配置防火墙或其他网络安全措施是您的工作，而不是我的工作。



@subsection{使用@tt{raco pollen} 启动项目服务器}

在我们启动项目服务器之前，先说一下@exec{raco pollen} 命令。

当您安装 Racket 时，Racket 安装了一个名为 @exec{raco} 的实用程序。这个名字是@bold{Ra}cket @bold{co}mmand 的缩写，@exec{raco} 充当了——你猜对了——Racket 命令的中心。您在第一次安装 Pollen 时使用了它：

@terminal{
> raco pkg install pollen
}

@exec{raco} 之后的第一个参数是子命令。例如，@exec{raco pkg ...} 允许您安装、更新和删除软件包，如下所示：

@terminal{
> raco pkg update pollen
> raco pkg remove pollen
}

同样，@exec{raco pollen} 允许您发出与 Pollen 相关的命令。 （有关完整列表，请参阅@secref["raco-pollen"]。）这是一个您可以尝试的简单方法：

@terminal{
> raco pollen version
@pollen:version
}

现在我们将使用@exec{raco pollen} 启动项目服务器。转到您的命令行并输入以下内容：

@terminal{
> cd /path/to/tutorial
> raco pollen start}

片刻之后，您将看到如下所示的启动消息：

@terminal{
Welcome to Pollen @pollen:version (Racket @(version))
Project root is /path/to/tutorial/
Project server is http://localhost:8080 (Ctrl+C to exit)
Project dashboard is http://localhost:8080/index.ptree
Ready to rock}

@italic{Project root} 表示项目服务器启动的目录，服务器将其视为其根目录。这意味着，绝对网址（即以 @litchar{/} 开头的网址）将解析到此目录中。所以像@tt{/styles.css} 这样的URL 将隐含地变成@tt{/path/to/tutorial/styles.css}。 

如果您使用裸命令@exec{raco pollen start}，项目服务器将在当前目录中启动。但是，如果您想在其他地方启动项目服务器，您可以将该目录作为参数添加，如下所示：

@terminal{
> raco pollen start /some/other/path
}

启动消息的下一行告诉你项目服务器的网址是@tt{http://localhost:8080}。这是您将放入 Web 浏览器以测试您的项目的地址。如果你不熟悉这种风格的 URL，@tt{localhost} 指的是你自己的机器，@tt{8080} 是项目服务器将响应浏览器请求的网络端口。

如果您想在一台机器上运行项目服务器并从另一台机器访问它（例如，出于测试目的），您不能使用@tt{localhost}。但是您可以使用运行项目服务器的机器的 IP 地址（例如，@tt{http://192.168.1.10:8080}）或可通过本地 DNS 获得的该机器的任何名称（例如，@tt{http://我的笔记本电脑:8080}）。

虽然端口@tt{8080}是默认的，但你可以通过在@exec{raco pollen start}中添加参数，在任何你喜欢的端口启动项目服务器。

@terminal{
> raco pollen start /path/to/tutorial
> raco pollen start /path/to/tutorial 8088
}
请注意，当您传递端口参数时，您还必须传递路径参数。 （没有它，你会得到一个错误，如下图所示。）如果你希望项目服务器在当前目录中启动，你可以使用通常的 @litchar{.} 简写：

@terminal{
> cd /path/to/tutorial
> raco pollen start 8088 
@racketerror{/path/to/tutorial/8088 is not a directory}
> raco pollen start . 8088 
Welcome to Pollen @pollen:version (Racket @(version)) ...
}

@margin-note{您还可以通过覆盖@racket[default-project-server-port] 或使用@racket[current-server-port] 对其进行参数化来更改项目服务器的默认端口。您还可以同时运行多个项目服务器——只需在不同的端口上启动它们，这样它们就不会相互冲突。}

您的终端窗口将在项目服务器运行时报告状态和错误消息。使用 @exec{ctrl+C} 停止服务器。


@subsection{项目仪表板}

对于项目中的每个目录，从顶部开始，项目服务器会在您的 Web 浏览器中显示一个 @defterm{dashboard}。仪表板为您提供目录中文件的概览以及查看它们的链接。

顶级仪表板的地址是@link-tt{http://localhost:8080/index.ptree}。其他仪表板遵循相同的模式（例如，@tt{http://localhost:8080/path/to/dir/index.ptree}。）

@margin-note{仪表板故意 @bold{不} 使用@tt{http://localhost:8080/} 或其等效项@tt{http://localhost:8080/index.html} 。为什么？因为这样，它不会干扰您可能希望在项目中拥有的任何 @tt{index.html}。}

因此，仪表板依赖于一个不同的文件，称为@filepath{index.ptree}。 @filepath{.ptree} 扩展名是 @defterm{pagetree} 的缩写。在 Pollen 中，页面树是页面的分层列表。我们将在后面的教程中对页面树做更多的事情。现在，请注意，要为仪表板创建文件列表，项目服务器将首先在每个目录中查找实际的 @filepath{index.ptree} 文件。如果找不到，它将从目录中的文件列表生成一个页面树。

让我们看看我们项目的根级仪表板。首先，确保您的项目服务器正在运行：

@terminal{
> cd /path/to/tutorial
> raco pollen start
}

然后，在您的网络浏览器中，访问@link-tt{http://localhost:8080/index.ptree}。

您应该看到如下内容：

@image/rp["dashboard.png"]

第一行告诉我们我们在项目的根目录中。我们没有创建一个显式的@filepath{index.ptree} 文件，所以项目服务器只是向我们显示了一个目录列表。


@subsection{Source files in the dashboard}

我们看到了唯一的文件，@filepath{poem.html.pp}。请注意，@filepath{.pp} 扩展名是灰色的。仪表板自动将对源文件和输出文件的引用合并到一个条目中。这个条目说的是 ``该目录包含输出文件@filepath{poem.html} 的@filepath{.pp} 格式的源文件。''

仪表板上的每个源文件都有三个链接。第一个链接附加到文件名本身，并带您预览输出文件。如果输出文件尚不存在（如此处所示），则会在您单击链接时动态呈现。 （无论您在仪表板中单击它的名称，还是从另一个页面链接到它，都是如此。）因此，单击文件名。您将在浏览器中看到：

@browser{
The margin is 42em. The border is red. The padding is 15em. The border is too.} 

当然，这是一个无聊的网页。这里的要点是您从源文件中看到了@italic{output}，它以前不存在。注意地址栏是@tt{http://localhost:8080/poem.html}，而不是@tt{poem.html.pp}。如果您查看@tt{tutorial} 目录，您会看到一个名为@filepath{poem.html} 的新文件。

换句话说，当您单击仪表板中的文件名链接时，Pollen 从源文件呈现输出文件并将其保存在您的项目目录中。如前所述，输出文件的名称 (@filepath{poem.html}) 是源文件的名称 (@filepath{poem.html.pp}) 减去 Pollen 扩展名 (@filepath{.pp})。

如果您返回仪表板并再次单击文件名链接，您将看到相同的输出文件。如果源文件没有改变，Pollen 只会显示已经渲染的输出文件。

但是，如果您愿意，可以在 DrRacket 中打开您的 @filepath{poem.html.pp} 源文件，编辑前两行，然后保存文件：

@fileblock["/path/to/tutorial/poem.html.pp" @codeblock|{
#lang pollen

The cave is pitch black.
Look out for the grue.
The padding is 15em.
The border is too.
}|]

返回仪表板并单击文件名。这一次，你会看到：

@browser{
The cave is pitch black. Look out for the grue. The padding is 15em. The border is too.} 

在这里，Pollen 注意到源文件已更改，因此它刷新了输出文件。这使得在 DrRacket 和您的网络浏览器之间工作、编辑源代码然后重新加载以查看更改变得很方便。

仪表板中的其他两个链接标记为@tt{in} 和@tt{out}。

标记为 @tt{in} 的链接将显示源文件的内容：

@terminal{
#lang pollen

The cave is pitch black.
Look out for the grue.
The padding is 15em.
The border is too.
}

标记为@tt{out} 的链接将显示输出文件的内容（就像您的网络浏览器中的“查看源代码”选项一样）：

@terminal{
The cave is pitch black.
Look out for the grue.
The padding is 15em.
The border is too.}

目前，除了@tt{#lang} 行之外，这些文件是相同的。但让我们改变一下。

@section{使用预处理器}

 Pollen 可以以多种模式运行。其中之一是 @defterm{预处理器} 模式。预处理器是一种用于对文件进行系统、自动更改的工具，通常考虑进一步处理（因此称为@defterm{pre-}）。您可以通过这种方式使用 Pollen 预处理器——作为处理管道的第一步。

或者你可以单独使用它，并让你的文件处于完成状态。这就是我们现在要使用它的方法。我们将构建我们的 @filepath{poem.html.pp} 源文件，这样，当它退出预处理器时，我们就有了一个合法的 HTML 文件。

@margin-note{有关 Pollen 处理模式的更多信息，请参阅@secref["file-types"]。}

@subsection{设置预处理器源文件}

Pollen 源文件的文件扩展名告诉 Pollen 应用什么样的处理。 @filepath{.pp} 文件扩展名代表``Pollen 预处理文件。'' 以下是如何将预处理器与任何基于文本的文件一起使用：

@itemlist[#:style 'ordered

@item{插入@tt{#lang pollen} 作为第一行。}

@item{添加@filepath{.pp} 文件扩展名。}

@item{通过 Pollen 运行它。}
]



``我可以将预处理器与@bold{任何} 类型的基于文本的文件一起使用吗？'' 当然 ``但是怎么做？'' 预处理器读取文件，处理它找到的任何 Pollen 命令，并让其余内容原封不动地通过。对于预处理器来说，这一切都只是文本数据。它不关心该文本是否代表 HTML、CSS、XML、LaTeX，甚至是 @link["https://en.wikipedia.org/wiki/TI-BASIC"]{TI-BASIC}。

让我们验证我们的源文件是否已为预处理器准备好：

@fileblock["/path/to/tutorial/poem.html.pp" @codeblock{
#lang pollen

The margin is 42em.
The border is red.
The padding is 15em.
The border is too.
}]

该文件以@tt{#lang pollen} 作为第一行，@filepath{.pp} 作为文件扩展名。所以是的——它满足预处理器的最低要求。

@subsection{创建有效的 HTML 输出}

让我们更新我们的源代码，使其生成有效的 HTML。编辑源如下：

@fileblock["/path/to/tutorial/poem.html.pp" @codeblock{
#lang pollen
<!DOCTYPE html>
<html>
<body>
<pre>
The margin is 42em.
The border is red.
The padding is 15em.
The border is too.
</pre>
</body>
</html>}]

返回项目服务器，查看@link-tt{http://localhost:8080/poem.html}。在之前，输出看起来像这样：

@browser{
The margin is 42em. The border is red. The padding is 15em. The border is too.} 


但是现在，由于@code{<pre>} 标签，这首诗将以等宽字体出现，并且将保留换行符：

@terminal{
The margin is 42em.
The border is red.
The padding is 15em.
The border is too.}

和以前一样，由于源文件已更改，Pollen 会刷新输出文件。在仪表板中，您可以使用 @tt{in} 和 @tt{out} 链接来检查源和输出。

现在这是一个有效的 HTML 页面。

@subsection{添加 Pollen 命令}

我提到预处理器读取文件并处理它找到的任何 Pollen 命令。但我们的源文件还没有任何命令。

让我们添加一些。源文件中的 Pollen 命令可以用以下两种方式之一编写：@defterm{ Pollen 风格}或@defterm{ Racket 风格}。我们将在下一个教程中尝试 Pollen 风格的命令。现在，我们将使用 Racket 风格。

要制作 Racket 风格的命令，只需使用任何 Racket 命令并将菱形字符 @litchar["◊"] 放在它前面。例如，这些是有效的 Racket 命令：

@codeblock{
#lang racket
(define inside 2)
(define edge (* inside 4))
(define color "blue")
}

这些是 Pollen 中等效的 Racket 式命令：

@codeblock{
#lang pollen
◊(define inside 2)
◊(define edge (* inside 4))
◊(define color "blue")
}


需要注意的是：由于预处理器只处理文本，你在预处理器中使用的Pollen命令必须产生文本值，或者可以合理地转换为文本。预处理器可以帮助你将基本的Racket值自动转换为文本：

@fileblock["odd-values.html.pp" @codeblock{
#lang pollen
◊(void)◊; 这将成为一个空字符串
◊(string->path "foo")
◊#\C
◊(sqrt 2) 
◊(list 1 2 3 4 5) 
◊(map sqrt (list 1 2 3 4 5))
 }]

@repl-output{
foo
C
1.4142135623730951
'(1 2 3 4 5)
'(1 1.4142135623730951 1.7320508075688772 2 2.23606797749979)}

如果您使用更复杂的数据类型，或者您不喜欢预处理器的自动转换，您可以手动将值转换为文本。

也就是说， Pollen 不会阻止你做这样的荒谬的事情：

@fileblock["bad-poem.html.pp" @codeblock{
#lang pollen

The cave is pitch black.
Look out for the grue.
◊(insert-mp3-recording-of-scream)
 }]

 Pollen 会尽力满足您的要求，但可能会发生奇怪的事情。结果肯定不是有效的 HTML，因为您不能简单地将二进制数据放在 HTML 文件的中间。 

正如巴贝奇先生警告的那样——垃圾进，垃圾出。


@subsection{ Racket 基础知识（如果你不熟悉）}

``但我从来没有用过 Racket 。'' 今天，你开始了。以下是 Racket 的四个基本规则：

@itemlist[#:style 'ordered

@item{Racket 的核心构建块是 @italic{expression}。表达式可以是单个值（如@racket[2] 或@racket{blue}）、变量（如@code{edge}）、值列表（如@racket[(list 2 "blue" edge) ])，或函数调用。}

@item{函数调用放在括号之间。与大多数语言不同，函数名称排在@italic{第一位}，然后是其参数（所以它是@racket[(* inside 4)]，而不是@racket[(inside * 4)]）。这称为@italic{前缀表示法}。}

@item{@italic{计算} 每个表达式以产生一个值。一个变量的计算结果是它持有的任何值（所以在我们说@code{（define inside 2）}之后，@code{inside} 将被计算为@racket[2]）。函数调用的计算结果为返回值（所以@racket[(+ 2 2)] 将计算为 @racket[4]）。}

@item{表达式可以包含递归嵌套的表达式。因此，@racket[(* inside 4)] 可以写成@racket[(* inside (+ 2 2))] 或@racket[(* inside (+ (+ 1 1) (+ 1 1)))]。}

]

@margin-note{刚接触 Racket 的人经常对前缀符号和括号耿耿于怀。如果你需要把它从你的系统中弄出来，那就去吧。但是请记住，这不是某种特殊的矫揉造作，而是规则#1 的必然结果。随着时间的推移，您会同意收益大于成本的。}

这就是你需要弄清楚下面的 Pollen 命令所发生的一切：

@codeblock{
#lang pollen
◊(define inside 2)
◊; 创建一个变量 `inside` 来保存数值2。
◊(define edge (* inside 4))
◊; 创建一个变量 `edge` ，其值是 `inside` 的四倍。
◊(define color "blue")
◊; 创建一个变量 `color` 保存值 “blue” 
}

要了解有关 Racket 语法的更多信息，请考虑通过出色的 @other-doc['(lib "scribblings/quick/quick.scrbl")]。


@subsection{使用 Racket 式命令定义变量}

让我们使用 Racket 样式的命令来定义变量，这些变量将为我们的页面保存一些值。首先，在源文件中添加一个@code{<head>} 标记，并添加三个命令来定义三个变量：

@fileblock["/path/to/tutorial/poem.html.pp" @codeblock{
#lang pollen

<!DOCTYPE html>
<html>
<head>
◊(define inside 2)
◊(define edge (* inside 4))
◊(define color "blue")
</head>
<body>
<pre>
The margin is 42em.
The border is red.
The padding is 15em.
The border is too.
</pre>
</body>
</html>}]

然后再看@link-tt{http://localhost:8080/poem.html}。它看起来是一样的吗？这不是一个骗人的问题——它应该就是这样。如果你点击仪表板上的@onscreen{Out}链接，你会看到这个：

@terminal{
<!DOCTYPE html>
<html>
<head>



</head>
<body>
<pre>
The margin is 42em.
The border is red.
The padding is 15em.
The border is too.
</pre>
</body>
</html>}

空行是怎么回事？不要惊慌——@tt{◊@racket[(define ...)]} 命令会创建一个变量，但命令本身不会计算出任何值。因此，我们得到了空行。 （也不要对此感到恐慌——@racket[define] 是所有表达式求值的一般规则的一个例外。）到目前为止，一切都很好。

@subsection{从变量中插入值}

要在我们的文件中插入变量的值，我们使用命令@litchar{◊|}@italic{variable-name}@litchar{|}。现在让我们这样做：

@fileblock["/path/to/tutorial/poem.html.pp" @codeblock{
#lang pollen

<!DOCTYPE html>
<html>
<head>
◊(define inside 2)
◊(define edge (* inside 4))
◊(define color "blue")
</head>
<body>
<pre>
The margin is ◊|edge|em.
The border is ◊|color|.
The padding is ◊|inside|em.
The border is too.
</pre>
</body>
</html>}]

在这里，我们将诗中的三个值替换为包含这些值的变量——@code{◊|edge|}、@code{◊|color|} 和 @code{◊|inside|}。 @link["http://localhost:8080/poem.html"]{重新加载文件} 在项目服务器中，你会看到：

@terminal{
The margin is 8em.
The border is blue.
The padding is 2em.
The border is too.}

嘿，看吧——诗的文本变了。现在它甚至押韵了。

如果您愿意，可以在源文件中编辑具有不同值的变量定义并在项目服务器中重新加载页面。该页面将使用新值重新呈现。特别是，如果您更新 @racket[inside]，您还会看到 @racket[edge] 发生变化，因为它的值取决于 @racket[inside]。

@margin-note{进一步提示：将变量作为值插入时，您也可以只输入@racket[◊variable-name]。两侧的垂直条永远不会出错，但只有当您希望该值与右侧的其他文本齐平时才强制使用。}

@subsection{在 CSS 中插入变量}

我们的诗现在对页面的@code{margin}、@code{border} 和@code{padding} 提出了不正确的声明。为了解决这个问题，我们将依赖在 HTML 文件中插入变量的相同基本技术。但我们不会将它们放在页面的@code{<body>} 中，而是将它们放在 CSS @code{<style>} 标记中。

使用新的@code{<style>} 标签更新页面的@code{<head>} 部分，该标签定义@code{pre} 的样式，如下所示，使用我们的变量获取相关值：


@fileblock["/path/to/tutorial/poem.html.pp" 
@codeblock{
#lang pollen

<!DOCTYPE html>
<html>
<head>
◊(define inside 2)
◊(define edge (* inside 4))
◊(define color "blue")
<style type="text/css">
pre {
    margin: ◊|edge|em;
    border: ◊|inside|em solid ◊|color|;
    padding: ◊|inside|em;
}
</style>
</head>
<body>
<pre>
The margin is ◊|edge|em.
The border is ◊|color|.
The padding is ◊|inside|em.
The border is too.
</pre>
</body>
</html>
}]

请注意，我们使用与之前相同的 @litchar{◊|}@italic{variable-name}@litchar{|} 模式来插入变量值。 

我们期望看到什么？我们预计 @code{padding} 和 @code{border} 将是 @code{2em} 宽，因为 @code{inside} 是 @code{2}。我们期望 @code{margin} 为 @code{8em}，因为它等于 @code{edge}，即 @code{inside} 乘以 @code{4}。我们期望边框的颜色是@racket["blue"]，因为这是变量 @code{color} 的值。

事实上，当你在项目服务器中 @link["http://localhost:8080/poem.html"]{reload the file} 时，你会看到：

@image/rp["result.png" #:scale 0.7]

和以前一样，如果您在源文件中编辑变量的值并返回到项目服务器，您将看到文本和布局都发生了变化。


@section{第一个教程完成}

这是一个鬼鬼祟祟的教程。虽然我们制作的 HTML 页面非常简单，但我们看到了 Pollen 开发环境的所有关键元素，并了解了预处理器。

随时返回并尝试您所学的内容。 @seclink["second-tutorial"]{下一个教程} 将假定您对这里的所有材料都十分熟悉。






