#lang scribble/manual

@(require "mb-tools.rkt" scribble/eval pollen/cache pollen/setup (for-label racket pollen/core pollen/setup pollen/render pollen/file sugar txexpr))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))

@title{Cache（缓存）}

@defmodule[pollen/cache]

Pollen @racket[render]最慢的部分是编译一个源文件。因为Pollen允许动态编辑和预览源文件，这些文件经常被重新编译。因此，Pollen将源文件的输出副本--即存储在@code[(format "~a" pollen-main-export)]和@code[(format "~a" pollen-meta-export)]中的任何内容--存储在一个缓存中，以便它们可以被重复使用。

在项目的每个目录中，Pollen 将缓存文件写入名为 @filepath{compiled} 的子目录中。这些文件存储在磁盘上，因此可以在会话之间重复使用。如果您删除缓存目录中的文件（或整个文件），请不要担心 - 一切都会重新生成。 （但是，我不建议尝试直接读取或写入任何 @filepath{compiled} 目录，因为缓存的实现细节可能会发生变化。）

@section{Preloading and reseting}

虽然缓存会在您使用 Pollen 时被填充，但您也可以使用 @exec{@seclink["raco_pollen_setup"]} 对其进行预热。此命令会将所有源文件加载到缓存中。这将在与项目服务器的交互会话期间为您提供最快速的性能。

如果要重建所有编译缓存，请使用 @exec{@seclink["raco_pollen_reset"]}。

@section{Disabling the cache}

编译缓存由@seclink["setup-overrides"]{overridable value} @racket[setup:compile-cache-active] 控制。因此，要禁用编译缓存，请将 @racket[setup] 子模块添加到您的 @filepath{pollen.rkt} 中，如下所示：

@codeblock|{
(module setup racket/base
  (provide (all-defined-out))
  (define compile-cache-active #f))
}|

Pollen也会缓存渲染的输出文件，所以如果你想禁用所有的缓存--从而迫使所有的东西每次都重新编译--你还应该通过覆盖@racket[setup:render-cache-active]禁用渲染缓存。

@codeblock|{
(module setup racket/base
  (provide (all-defined-out))
  (define compile-cache-active #f)
  (define render-cache-active #f))
}|

请注意，这会使您的渲染速度变慢。但是会保证您每次都完全重新编译，这在开发中有时会很有用。


@section{Scope of dependency tracking}

编译缓存跟踪源文件的修改日期、@secref["The_POLLEN_environment_variable"] 的当前设置以及模板和@filepath{pollen.rkt} 的修改日期（如果它们存在）。对于@tt{poly} 源文件，它还跟踪@racket[current-poly-target]。它还跟踪您在可选设置值@racket[setup:cache-watchlist] 中列出的文件和在可选设置值@racket[setup:envvar-watchlist] 中列出的环境变量。

但是，它不会跟踪所有可能的依赖关系。因此，在一个复杂的项目中，可能会创建缓存未注意到的深层依赖关系。尤其要注意的是，Pollen 不会将 pagetree 文件作为其他源文件的依赖项进行跟踪。因此，如果您更改页面树，您通常需要使用 @exec{raco pollen reset} 来清除缓存。

不幸的是，没有办法解决这个问题。为了使缓存有用，依赖检查的范围必须受到限制。为了捕获所有可能的依赖关系，缓存每次都必须重新编译每个文件——这相当于根本不缓存。

那些需要这种深度动态的人可以禁用缓存（使用设置值@racket[setup:render-cache-active] 和@racket[setup:compile-cache-active]）。


@section[#:tag-prefix "cache"]{Functions}


@deftogether[(
@defproc[
(cached-doc
[source-path pathish?])
txexpr?]

@defproc[
(cached-metas
[source-path pathish?])
hash-eq?]
)]
尝试从缓存中检索请求的值。如果它不存在或已过期，则使用 @racket[dynamic-require] 从源更新它。

这些函数是@racket[get-doc] 和@racket[get-metas] 的低级表亲，具有更方便的接口。除非您有特殊原因，否则最好使用它们。

如果你想要获得缓存的速度优势，你应该使用@racket[cached-doc] 和@racket[cached-metas] 从 Pollen 源文件中获取数据，而不是使用像@racket[require]、@racket[local -require] 和 @racket[dynamic-require]这样的函数。这些也将起作用。他们只会慢一点。


@defproc[
(reset-cache)
void?]
Clears the cache. When only the nuclear option will do.
