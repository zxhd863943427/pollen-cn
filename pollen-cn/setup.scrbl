#lang scribble/manual
@(require "mb-tools.rkt")
@(require scribble/eval pollen/setup racket/string (for-label (except-in racket #%top) racket/runtime-path syntax/modresolve (except-in pollen #%module-begin #%top) pollen/render pollen/setup pollen/top))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/setup))

@(require (for-syntax racket/base racket/syntax pollen/setup))
@(define-syntax (defoverridable stx)
  (syntax-case stx ()
    [(_ name predicate? desc ...)
     (with-syntax* ([default-name (format-id #'here "default-~a" #'name)]
                   [value (let ([v (syntax-local-eval #'default-name)])
                            (cond
                              [(and (list? v) (andmap symbol? v) (> (length v) 5)) #`'#,'(see below)]
                              [(or (symbol? v) (list? v)) #`'#,v]
                              [(procedure? v) '(λ (path) #f)]
                              [else v]))]
                   [setup:name (format-id stx "setup:~a" #'name)])
       #`(deftogether ((defproc (setup:name) predicate?)
                       (defthing default-name predicate? #:value value))
           desc ...))]))

@title{Setup}

@defmodule[pollen/setup]


@section[#:tag "setup-overrides"]{How to override setup values}

可以通过在 @racket[“poly.rkt”] 源文件中重写以下值来更改这些值：

@itemlist[#:style 'ordered

@item{在这个文件中，@seclink["submodules" #:doc '(lib "scribblings/guide/guide.scrbl")]{创建一个名为 @racket[setup] 的子模块}。}

@item{在这个子模块中，使用 @racket[define] 来创建一个与 @racket[pollen/setup] 中的变量同名但没有 @racket[setup:] 前缀的变量。}

@item{给它分配你喜欢的任何值。}

@item{根据需要重复。}

@item{(别忘了 @racket[provide] @racket[setup] 子模块中的变量。)}

 ]

当 Pollen 运行时，这些定义将取代 @racketmodname[pollen/setup] 中的定义。

例如，假设您希望将每个 Pollen 源文件的主要导出称为 @racket[van-halen] 而不是 @racket[doc]， Pollen 标记文件的扩展名是 @racket[.rock] 而不是 @racket[.pm]，命令字符是 @litchar{🎸} 而不是 @litchar{◊} 。你的 @racket["pollen.rkt"] 会看起来像这样：

@fileblock["pollen.rkt" 
@codeblock{
#lang racket/base

;; ...通常的定义和标签函数...

(module setup racket/base
  (provide (all-defined-out))
  (define main-export 'van-halen)
  (define markup-source-ext 'rock)
  (define command-char #\🎸))
}]

当然，您可以通过从 @racket["pollen.rkt"] 中删除这些定义的值来恢复默认值。

每个@racket[setup:]@racket[_name] 函数都会解析该变量的当前值：它将从 @racket[setup] 子模块返回值（如果此处定义了 @racket[_name] ），否则它将返回默认值（可直接从@racket[default-]@racket[_name] 中获得）。例如， @racket[default-command-char] 将始终为 @litchar{◊} ，但在上面的示例中， @racket[(setup:command-char)] 将返回 @litchar{🎸} 。

@section{Values}

@defoverridable[project-server-port integer?]{
确定项目服务器的默认 HTTP 端口。}



@defoverridable[main-pagetree string?]{Pollen 仪表板在每个目录中默认加载的页面树。}



@defoverridable[main-root-node symbol?]{解码后的 @racket[doc] 中根节点的名称。}

@defoverridable[block-tags (listof symbol?)]{被 @racket[block-txexpr?] 视为块的标签。初始化为@link["https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements"]{HTML5中的块级元素}，即：

@racketidfont{@(string-join (map symbol->string (cdr default-block-tags)) " ")}

... 拓展 @racket[setup:main-root-node].}



@defoverridable[command-char char?]{表示 Pollen 命令、函数或变量的魔法字符。}


@deftogether[(
@(defoverridable newline string?)
@(defoverridable linebreak-separator string?)
@(defoverridable paragraph-separator string?)
)]    
解码中使用的默认分隔符。


@defoverridable[render-cache-active boolean?]{通过重用 Pollen 输出文件的渲染版本来加速交互会话的渲染缓存是否处于活动状态。}

@defoverridable[compile-cache-active boolean?]{通过保存 Pollen 源文件的编译版本来加速交互会话的编译缓存是否处于活动状态。}

@defoverridable[compile-cache-max-size exact-positive-integer?]{编译缓存的最大大小。}

@defoverridable[cache-watchlist (listof (or/c path?path-string?))]{缓存（= 渲染缓存 + 编译缓存，统称）在项目服务器会话期间监视的额外文件列表。如果监视列表中的文件之一发生更改，则缓存无效（就像 @racket["pollen.rkt"] 更改时一样）。

如果缓存在监视列表中找不到某个文件，则不会出现错误。该文件将被忽略。因此，为避免意外行为，最好的策略是使用完整路径（或路径字符串）。生成本地文件完整路径的一种方法是使用 @racket[define-runtime-path] 。另一种方法是，如果您正在使用已作为包的一部分安装的模块，则使用 @racket[resolve-module-path]：

@fileblock["pollen.rkt" 
@codeblock{
(module+ setup
  (provide (all-defined-out))
  (require racket/runtime-path syntax/modresolve)
  (define-runtime-path my-local-mod "my-module.rkt")
  (define my-installed-mod (resolve-module-path 'package/my-other-module))
  (define cache-watchlist (list my-local-mod my-installed-mod)))
}]

@history[#:added "1.4"]
}

@defoverridable[envvar-watchlist (listof string?)]{缓存键中使用的额外环境变量列表。将为环境变量的每个不同值维护单独的缓存。 @secref["The_POLLEN_environment_variable"] 总是被使用，不管这个值是如何设置的。

环境变量的名称和值都不区分大小写，因此 @racket["PUB"] 和 @racket["pub"] 和 @racket["pUb"] 都被视为相同的变量。

@history[#:added "2.1"]}



@defoverridable[publish-directory (or/c path-string? path-for-some-system?)]{@secref{raco_pollen_publish} 的默认目标。按原样使用完整路径；将相对路径发布到桌面.. @history[#:added "1.1"]}

@defoverridable[unpublished-path? (path? . -> . boolean?)]{@history[#:changed "1.1" @elem{已弃用。请使用 @racket[setup:omitted-path?].}]}


@defoverridable[omitted-path? (path? . -> . boolean?)]{确定是否从 @secref{raco_pollen_render} 和 @secref{raco_pollen_publish} 操作中省略路径的谓词。如果谓词的计算结果为 @racket[#t] ，则省略路径。

@history[#:added "1.1"]}

@defoverridable[extra-published-path? (path? . -> . boolean?)]{@history[#:changed "1.1" @elem{已弃用。请使用 @racket[setup:extra-path?].}]}

@defoverridable[extra-path? (path? . -> . boolean?)]{确定路径是否被渲染和发布的谓词，覆盖上面的 @racket[(setup:omimitted-path?)] 和 Pollen 的默认发布设置。例如，Pollen 会自动忽略扩展名为 @racket[.rkt] 的文件。如果您想强制发布 @racket[.rkt] 文件，您可以在此处包含它。

@history[#:added "1.1"]}


@defoverridable[poly-targets (listof symbol?)]{表示 @racket['poly] 源文件的可能目标的符号列表。}


@defoverridable[index-pages (listof string?)]{项目服务器将用作目录默认页面的字符串列表，按优先级排序。对命令行渲染操作没有影响。对您的实时 Web 服务器也没有影响（通常这是您需要在 @tt{.htaccess} 配置文件中进行的设置）。} 但是使用此设置，您可以模拟实时服务器的行为，以便内部 index-page URL 工作正常。

 @defoverridable[trim-whitespace? boolean?]{控制 Pollen 源阅读器是否从 @racket[doc] 导出的开头修剪空白的谓词。如果您将 Pollen 用作另一种编程语言的预处理器并且想要准确地保留前导空格，则可以将其设置为 @racket[#false]。

 @history[#:added "1.5"]}

@defoverridable[allow-unbound-ids? boolean?]{通过改变 @racketmodname[pollen/top] 中 @racket[#%top] 的行为来控制Pollen 是否将未绑定标识符转换为默认标签的谓词。

@history[#:added "2.0"]}

@defoverridable[external-renderer (or/c (list/c module-path? symbol?) #f)]{一个模块路径和标识符（适合与 @racket[dynamic-require] 一起使用），提供一个函数供 Pollen 在渲染 @seclink["Using_the_project_server"]{project server} 所需的文件时调用而不是 @racket[render] 或者在运行 @secref["raco_pollen_render"] 时。该函数必须接受与 @racket[render-to-file] 相同的参数，并且应该以 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{string} 或 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{byte string} 的形式返回最终输出。 Pollen 将始终为您将此返回值写入输出文件。

设置此值使您可以完全控制（并负责）Pollen 如何将编译的 @racketidfont{doc} 和 @racketidfont{metas} 从源文件转换为最终输出。您的渲染器应该能够处理 Pollen 的任何 @seclink["Source_formats"]{source formats} 或 @seclink["Utility_formats"]{utility formats}。 Pollen 的 @racket[render] 函数的操作不受设置此值的影响，因此您的渲染器可以将其用作后备。

 @history[#:added "3.2"]}
  
@section{Parameters}

我的意思是 Racket 意义上的 @italic{parameters} ，即可以提供给 @racket[parameterize] 的值。

@defparam[current-server-port port integer? #:value default-project-server-port]{
设置项目服务器的 HTTP 端口的参数。}


@defparam[current-project-root path path?]{
保存当前项目根目录的参数（例如，您启动 @code{raco pollen start} 的目录）。}


@defparam[current-server-extras-path dir path? #:value #f]{
报告项目服务器支持文件目录路径的参数。}

@defparam[current-poly-target target symbol? #:value 'html]{
报告 @racket[poly] 源文件的当前渲染目标的参数。}

