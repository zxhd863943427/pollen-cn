#lang scribble/manual

@(require "mb-tools.rkt" scribble/eval pollen/setup (for-label (except-in racket ...) pollen/setup))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))


@title[#:tag "raco-pollen"]{使用 @exec{raco pollen} 命令}


Racket 通过 @exec{raco} 提供集中的命令行选项（@exec{racket command} 的缩写，参见@other-doc['(lib "scribblings/raco/raco.scrbl")]）。

安装 Pollen 后，您可以使用子命令 @exec{raco pollen} 通过 @racket[raco] 访问以下特定的 Pollen 命令。

@section{Making sure @exec{raco pollen} works}

打开终端窗口并输入：

@terminal{
> raco pollen test}

如果@exec{raco pollen} 安装正确，您将看到：

@terminal{raco pollen is installed correctly}

但如果你得到：

@terminal{raco: Unrecognized command: pollen}

你需要在继续之前解决这个问题，很可能是重新安装 Pollen（请参阅 @secref["Installation" #:doc '(lib "pollen/scribblings/pollen.scrbl")]）。

如果你的错误是这样的：

@terminal{Unrecognized command: raco}

您的 Racket 安装存在更深层次的问题（通常是 @code{PATH} 的错误配置）。

@section{@exec{raco pollen}}

与@exec{raco pollen help} 相同。

@section{@exec{raco pollen help}}

显示可用命令的列表。


@section{@exec{raco pollen start}}

从当前目录启动项目服务器，使用默认端口，即参数 @racket[current-server-port] 的值（默认为端口 @id[default-project-server-port] ）。


可以使用两个可选参数和两个可选开关调用此命令。

@racket[raco pollen start _path] 将从@racket[_path] 而不是当前目录启动项目服务器（使@racket[_path] 成为其根目录）。

@terminal{
> raco pollen start ~/path/to/project/}

@racket[raco pollen start _path _port] 将使用@racket[_port] 而不是@racket[current-server-port] 在@racket[_path] 中启动项目服务器。如果您想同时运行多个项目服务器，这很有用。

@terminal{
> raco pollen start ~/path/to/project/
> raco pollen start ~/path/to/project/scribblings 8088}

如果你想在当前目录下启动，但使用不同的端口，请使用 @litchar{.} 作为路径。

@terminal{
> raco pollen start . 8088}


@margin-note{Pollen 默认使用端口 @id[default-project-server-port] 因为其他网络服务不常用它。但是 Pollen 不知道您的机器上还运行着什么。如果@id[default-project-server-port] 已经在使用中，当您尝试启动 Pollen 项目服务器时会出现错误。在这种情况下，请尝试不同的端口。}


添加可选的 @exec{-l} 或 @exec{--launch} 开关将在项目服务器启动后在 Web 浏览器中打开主项目仪表板。

添加可选的 @exec{--local} 开关将限制项目服务器对来自 localhost 的请求做出响应。(默认情况下，项目服务器将响应来自任何客户端的请求）。


@section{@exec{raco pollen render}}

该命令可以通过两种方式调用：源文件模式或目录模式。

在这两种模式下，可选的 @exec{--dry-run} 或 @exec{-d} 开关会打印出该命令所渲染的路径，而无需实际执行。

在这两种模式下，可选的 @exec{--force} 或 @exec{-f} 开关会通过更新文件的修改日期（类似于 @exec{touch} ），强制从源头进行新的渲染，即使该文件已经被缓存了。因此，如果修改日期对你很重要，不要使用这个选项。

在这两种模式下，可选的 @exec{--null} 或 @exec{-n} 开关会像往常一样渲染，但不写入任何文件。(如果你要安排特殊的渲染行为，例如写到数据库或网络服务器，那就很方便了）。



@bold{Source 模式}：@racket[raco pollen render _source ...] 将仅渲染 @racket[_source ...] 中指定的源路径。与通常的命令行习惯用法一致，这可以是单个路径、路径列表或模式：

@terminal{
> raco pollen render foo.html.pm
> raco pollen render foo.html.pm bar.html.pm zam.css.pp
> raco pollen render *.html.pm}

路径也可以被指定为输出而不是输入路径，相应的源文件路径将被发现：

@terminal{
> raco pollen render foo.html
> raco pollen render foo.html bar.html zam.css}

如果在 @racket[_source] 中包含了一个 pagetree 文件，它所列出的所有文件都将使用上述规则进行渲染。

可选的 @exec{--target} 或 @exec{-t} 开关指定了用于多输出源文件的渲染目标。(在 @racket[_source] 中遇到的其他类型的文件仍然会照常渲染。) 如果省略了目标，渲染器将使用 @racket[(setup:poly-targets)] 中首先出现的目标。

@terminal{
> raco pollen render -t pdf foo.poly.pm}

另见 @seclink["raco-pollen-render-poly"] 。

可选的 @exec{--parallel} 或 @exec{-p} 开关会创建一组与系统中处理核心数量相等的并行渲染作业。在多核机器上，这通常会使你的渲染工作更快地完成。当然，渲染的顺序是不能保证的，所以如果你的项目依赖于一定的渲染顺序，不要使用这个选项。

@terminal{
> raco pollen render -p foo.html bar.html zam.css
}

替代的 @exec{--jobs <count>} 或 @exec{-j <count>} 开关做同样的事情，但需要一个参数来创建 @racket[<count>] 并行作业（可以多于或少于处理核心的数量）。

@terminal{
> raco pollen render -j 4 foo.html bar.html zam.css
}

根据经验，如果你先执行 @exec{raco setup} ，更新 Pollen 的磁盘缓存，那么并行渲染的效果最好：

@terminal{
> raco setup -p
> raco pollen render -p 
}

@italic{Warning}：在所有情况下，新渲染的输出文件将覆盖任何以前的输出文件。

@bold{目录模式}: @racket[raco pollen render _directory] 渲染所有预处理器的源文件，然后渲染在指定目录下发现的所有 pagetree 文件。如果没有找到这些文件，将为该目录生成一个 pagetree（它将包括所有的源文件，但也包括那里存在的所有其他文件；见 @secref["The_automatic_pagetree"] ），然后进行渲染。如果省略了 @racket[_directory] 参数，该命令默认为当前目录。

在目录模式下，这个命令可以用另外两个可选参数来调用（除了上面提到的 @exec{--target} 、 @exec{--parallel} 和 @exec{--jobs} 开关之外）。

@exec{--subdir}或 @exec{-s} 开关也会渲染子目录。 @racket[current-project-root] 仍然固定在初始目录，就像调用@racket[raco pollen start]后在项目服务器中的情况。

某些子目录会被自动忽略，包括 Racket 和 Pollen 的私有目录（如 @tt{compiled} ）和源代码控制目录（如 @tt{.git} 和 @tt{.svn} ）。你可以通过覆盖 @racket[default-omitted-path?] 省略其他路径。你可以通过覆盖 @racket[default-extra-path?] 来覆盖这些省略——也就是说，在递归渲染中强制包含一个路径。

@exec{--recursive}或 @exec{-r} 开关会递归地渲染子目录。意思是，每个子目录被当作一个独立的子项目， @racket[current-project-root] 会相应地移动。在许多项目中， @exec{-s} 和 @exec{-r} 开关之间没有任何区别。但如果在你的项目中，这种区别很重要的话，你可以同时拥有它们。




@section{@exec{raco pollen publish}}

在桌面上复制项目目录，但没有任何源文件或其他 Pollen 相关文件。 （这个功能比较鸡肋，请大家提出改进意见。）

@racket[raco pollen publish _project-dir] 会将@racket[_project-dir] 中的项目发布到桌面上名为@racket[publish] 的文件夹中。 @bold{警告}：如果桌面上已经存在 @racket[publish] ，它将被覆盖。

@racket[raco pollen publish _project-dir _dest-dir] 会将@racket[_project-dir] 中的项目发布到@racket[_dest-dir] 而不是桌面。 @bold{警告}：如果 @racket[_dest-dir] 已经存在，它将被新发布的目录覆盖。

如果你已经在你的项目目录中，并且想在桌面以外的地方发布，使用@racket[raco pollen publish _. _dest-dir]。

默认情况下，此命令将自动覆盖目标目录。添加可选的 @exec{-c} 或 @exec{--confirm} 开关将要求确认目标文件是否已经存在。

您可以通过覆盖 @racket[default-publish-directory] ​​来确定项目的默认发布目标。

某些文件和目录会自动从发布目录中省略，包括 Racket 和 Pollen 源、Pollen 缓存和源控制目录（如 @tt{.git} 和 @tt{.svn}）。您可以通过覆盖 @racket[default-omitted-path?] 来省略其他文件。您可以通过覆盖 @racket[default-extra-path?] 来覆盖这些遗漏——即强制发布路径。

可选的 @exec{--dry-run} 或 @exec{-d} 开关会打印用于发布的源目录和目的目录，而无需实际这样发布。如果不能创建目标目录路径，就会出现错误。

@section{@exec{raco pollen setup}}

在当前目录中查找 Pollen 源文件，对其进行编译，并将结果加载到 @seclink["Cache" #:doc '(lib "pollen/scribblings/pollen.scrbl")] 中。这将在与项目服务器的交互会话期间为您提供最快速的性能。

也可以作为@racket[raco pollen setup _directory] ​​调用，它将设置@racket[_directory] ​​中的文件。

可选的 @exec{--parallel} 或 @exec{-p} 开关会创建一组与系统中处理核心数量相等的并行设置作业。在多核机器上，这通常会使你的设置更快地完成。

@terminal{
> raco pollen setup -p
}

替代的 @exec{--jobs <count>} 或 @exec{-j <count>} 开关做同样的事情，但需要一个参数来设置 @racket[<count>] 并行作业的数量（可以多于或少于处理核心的数量）。

@terminal{
> raco pollen setup -j 4
}

@margin-note{截至 2020 年中期，Pollen 在 Racket 的 CS（= Chez Scheme）变体下的并行处理性能比普通 Racket 差。如果你使用 Racket CS，使用 @exec{-j 4} （将操作限制在四个核心）可能会比 @exec{-p} （将使用所有可用核心）得到更好的结果。}

可选的 @exec{--dry-run} 或 @exec{-d} 会开关打印将由该命令编译的路径，而不实际这样做。


@section{@exec{raco pollen reset}}

重置 Pollen 的 @seclink["Cache" #:doc '(lib "pollen/scribblings/pollen.scrbl")] ，删除缓存目录，包括 Racket 创建的 @tt{compiled} 目录。当你需要一个新的生成起点时，请使用这个方法。

也可以作为@racket[raco pollen reset _directory] ​​调用，这将会把一个不同目录 @racket[_directory] 的项目重置。

@section{@exec{raco pollen version}}

你相信吗，这会打印 Pollen 的版本号。

@section{The @exec{POLLEN} environment variable}

Pollen 能在命令行中识别一个 @exec{POLLEN} 环境变量，它可以用来传递你喜欢的任何值。这个值可以在你的项目文件中使用@racket[(getenv "POLLEN")]设置，如果没有设置，返回 @racket[#f] 。以这个文件为例：

@fileblock["test.txt.pp" @codeblock{
#lang pollen
Result is ◊or[(getenv "POLLEN")]{nothing}
}]

这个 @exec{POLLEN} 环境变量将改变它的渲染结果。

@terminal{
> raco pollen render test.txt ; cat test.txt
rendering test.txt.pp
rendering: /test.txt.pp as /test.txt
Result is nothing

> POLLEN=DEBUG raco pollen render test.txt ; cat test.txt
rendering test.txt.pp
rendering: /test.txt.pp as /test.txt
Result is DEBUG

}

@section{Logging & the @exec{PLTSTDERR} environment variable}

@margin-note{参见@secref["logging" #:doc '(lib "scribblings/reference/reference.scrbl")] 了解 Racket 的日志记录系统。}

默认情况下，Pollen 将在任何终端会话（例如，项目服务器或渲染作业）期间将 @racket['info] 级别或更高级别的消息记录到控制台。因此，如果您像这样启动项目服务器：

@terminal{
> raco pollen start
} 

你会看到以下列内容开始的日志信息：

@terminal{
pollen: starting project server ...
}

等等。

您可以使用 Racket 的 @racket[PLTSTDERR] 环境变量来调整日志记录的级别。如果您为 Pollen 提供明确的日志级别，它将覆盖此默认行为。因此，如果您只想查看 @racket['error] 级别或更高级别的消息，您可以像这样调用项目服务器：

@terminal|{
> PLTSTDERR=error@pollen raco pollen start
}|

在此之后，项目服务器将正常工作，但您不会看到通常的@racket['info] 级别的消息，而只会看到@racket['error] 或更高级别的消息。

相反，如果您想要更详细的日志记录，您可以调用 @racket['debug] 日志级别，如下所示：

@terminal|{
> PLTSTDERR=debug@pollen raco pollen start
}|

然后你会看到通常的 @racket['info] 信息，再加上一堆其他信息。

