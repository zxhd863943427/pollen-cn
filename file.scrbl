#lang scribble/manual

@(require scribble/eval "mb-tools.rkt" pollen/render pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/setup sugar pollen/file))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))

@title[#:tag "file-types"]{File}

@defmodule[pollen/file]

一个实用模块，提供了处理 Pollen 源文件和输出文件的功能。在普通使用中，你可能不需要这些。但如果你想对 Pollen 进行更精细的处理，它们就在这里。

Pollen 可以处理六种源文件：


@itemlist[
@item{ @bold{Preprocessor} ，文件扩展名为 @ext[pollen-preproc-source-ext]}

@item{ @bold{Markup} ，文件扩展名为 @ext[pollen-markup-source-ext]}

@item{ @bold{Markdown} ，文件扩展名为 @ext[pollen-markdown-source-ext]}

@item{ @bold{Null} ，文件扩展名为 @ext[pollen-null-source-ext]}

@item{ @bold{Scribble} ，文件扩展名为 @ext[pollen-scribble-source-ext]}

@item{ @bold{Pagetree} ，文件扩展名为 @ext[pollen-pagetree-source-ext]。这是唯一不生成输出文件的源类型。}

]


这个模块中的函数依赖于 @racketmodname[pollen/setup] 中指定的文件扩展。这些扩展名可以在一个项目中被覆盖--见 @secref["setup-overrides"] 。

对于每种 Pollen 源文件，对应的输出文件名是通过从源文件名中去掉扩展名而得到的。所以预处理器源文件 @filepath{default.css.pp} 会变成 @filepath{default.css} 。 (另见
@secref["Saving___naming_your_source_file"] ，如果这一点没有印象的话。)

Scribble 文件的工作方式不同--相应的输出文件是源文件，但扩展名为 @filepath{html} ，而不是 @filepath{scrbl} 。所以 @filepath["pollen.scrbl"] 会变成 @filepath["pollen.html"] 。

关于 Pollen 的文件模型的更多信息，请参见 @secref["File_formats"] 。

@deftogether[
(@defproc[
(preproc-source?
[val any/c]) 
boolean?]

@defproc[
(markup-source?
[val any/c]) 
boolean?]

@defproc[
(markdown-source?
[val any/c]) 
boolean?]

@defproc[
(null-source?
[val any/c]) 
boolean?]

@defproc[
(scribble-source?
[val any/c]) 
boolean?]

@defproc[
(pagetree-source?
[val any/c]) 
boolean?]
)]
根据文件扩展名，测试 @racket[_val] 是否是代表指定类型的源文件的路径。不检查 @racket[_val] 是否存在。

@examples[#:eval my-eval
(preproc-source? "main.css.pp")
(markup-source? "default.html.pm")
(markdown-source? "default.html.pmd")
(null-source? "index.html.p")
(scribble-source? "file.scrbl")
(pagetree-source? "index.ptree")
]


@deftogether[
(@defproc[
(->preproc-source-path
[p pathish?]) 
path?]

@defproc[
(->markup-source-path
[p pathish?]) 
path?]

@defproc[
(->markdown-source-path
[p pathish?]) 
path?]

@defproc[
(->null-source-path
[p pathish?]) 
path?]

@defproc[
(->scribble-source-path
[p pathish?]) 
path?]
)]
将输出路径 @racket[_p] 转换为产生该输出路径的指定类型的源路径。这个函数只是生成了一个相应的源路径--它并不询问这个源路径是否存在。(如果你想保证文件的存在，请使用 @racket[get-source] )。

@examples[#:eval my-eval
(define name "default.html")
(->preproc-source-path name)
(->markup-source-path name)
(->markdown-source-path name)
(->scribble-source-path name)
(->null-source-path name)
]


@deftogether[(

@defproc[
(get-source
[p pathish?]) 
(or/c #f path?)]

@defproc[
(get-markup-source
[p pathish?]) 
(or/c #f path?)]

@defproc[
(get-markdown-source
[p pathish?]) 
(or/c #f path?)]

@defproc[
(get-preproc-source
[p pathish?]) 
(or/c #f path?)]


@defproc[
(get-null-source
[p pathish?]) 
(or/c #f path?)]

@defproc[
(get-scribble-source
[p pathish?]) 
(or/c #f path?)]
)]
查找将产生输出路径 @racket[_p] 的现有源路径。

综合 @racket[get-source] 将按以下顺序检查源格式： @racket[get-markup-source] 、 @racket[get-markdown-source] 、 @racket[get-preproc-source] 、@racket[ get-null-source] 和 @racket[get-scribble-source]。

当然，特定类型的变体将只返回一个指定类型的源文件。

在所有情况下，如果没有相应的源文件，则返回 @racket[#f] 。




@defproc[
(->output-path
[p pathish?]) 
path?]
将一个源路径 @racket[_p] 转换为其对应的输出路径。这个函数只是为一个文件生成一个路径--它并不询问该文件是否存在。

如果 @racket[_p] 有 @seclink["The_poly_output_type"]{@id[pollen-poly-source-ext] 输出类型} ，那么 @racket[->output-path] 将使用 @racket[current-poly-target] 作为输出路径扩展。

否则，这个函数没有特定类型的变体，因为 Pollen 源文件的输出路径是 @seclink["Saving___naming_your_source_file"]{由其名称决定的} 。

@examples[#:eval my-eval
(->output-path "main.css.pp")
(->output-path "default.html.pm")
(->output-path "index.html.p")
(->output-path "file.scrbl")
]
