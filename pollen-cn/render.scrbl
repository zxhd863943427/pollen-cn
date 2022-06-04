#lang scribble/manual

@(require scribble/eval pollen/render pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/setup pollen/core web-server/templates pollen/file sugar pollen/render))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))

@title{Render}

@defmodule[pollen/render]

@italic{Rendering} 是 Pollen 源文件转换为输出的方式。

@defproc[
(render
[source-path complete-path?]
[template-path (or/c #f complete-path?) #f]) 
(or/c string? bytes?)]
渲染 @racket[_source-path] 。渲染行为取决于源文件的类型（有关详细信息，请参阅@secref["File_formats" #:doc '(lib "pollen/scribblings/pollen.scrbl")]）：

在没有模板的情况下渲染 @racketmodname[pollen/pre] 文件。

使用模板渲染 @racketmodname[pollen/markup] 或 @racketmodname[pollen/markdown] 文件。如果没有使用 @racket[_template-path] 指定模板，Pollen 会尝试使用 @racket[get-template-for] 找到模板。

请注意，使用模板进行渲染在 @racket[eval] 中使用 @racket[include-template] 。对于复杂的页面，第一次可能会很慢。缓存用于使后续请求更快。

对于那些对使用 @racket[eval] 感到恐慌的人，请不要这样做。正如 @racket[include-template] 的作者已经建议的那样，``如果你坚持动态主义''——是的，我坚持——``@link["http://docs.racket-lang.org/web-server/faq.html#%28part._.How_do_.I_use_templates__dynamically__%29"]{总是会有 @racket[eval] 的。}''

@defproc[
(render-to-file
[source-path complete-path?]
[template-path (or/c #f complete-path?) #f]
[output-path (or/c #f complete-path?) #f]) 
void?]
与 @racket[render] 类似，但将文件保存到 @racket[_output-path] ，覆盖已经存在的所有内容。如果没有提供 @racket[_output-path] ，它是使用 @racket[->output-path] 从 @racket[_source-path] 派生的。

@defproc[
(render-to-file-if-needed
[source-path complete-path?]
[template-path (or/c #f complete-path?) #f]
[output-path (or/c #f complete-path?) #f]) 
void?]
与 @racket[render-to-file] 类似，但仅当存在以下条件之一时才会进行渲染：
@itemlist[#:style 'ordered

@item{ @racket[_output-path] 处不存在文件。 （因此，强制渲染特定 @racket[_output-path] 的一种简单方法是删除它。）}

@item{自上次通过 @racket[render] 以来， @racket[_source-path] 、 @racket[_template-path] 或关联的 @filepath["pollen.rkt"] 发生了变化。}

@item{渲染缓存已停用。}]

如果这些条件都不存在， @racket[_output-path] 被认为是最新的，并跳过渲染。




@defproc[
(render-batch
[source-path pathish?] ...) 
void?]
一次性渲染多个 @racket[_source-paths] 。如果您的 @racket[_source-paths] 依赖于一组通用模板，这可能比 @racket[(for-each render _source-paths)] 更快。模板可能有自己的需要编译的源文件。如果您使用 @racket[render] ，模板将被重复（并且不必要地）重新编译。而如果你使用 @racket[render-batch] ，每个模板只会被编译一次。

@defproc[
(render-pagenodes 
[pt-or-pt-source (or/c pathish? pagetree?)]) 
void?]
使用 @racket[_pt-or-pt-source] ，使用 @racket[render-batch] 渲染该页面树中的页面节点。

请注意， @racket[_pt-or-pt-source] 严格作为一个要渲染的文件列表使用的，就像一个批处理文件。它并不被用作渲染文件的导航页树。

@defproc[
(get-template-for
[source-path complete-path?])
(or/c #f complete-path?)]
查找 @racket[_source-path] 的模板文件，优先级如下：
@itemlist[#:style 'ordered

@item{如果 @racket[_source-path] 的 @racket[metas] 有@code[(format "~a" pollen-template-meta-key)] 的键，则使用该键的值，例如—

@code{◊(define-meta template "my-template.html")}

如果你的项目有@seclink["fourth-tutorial"]{多个输出目标}，你可以提供一个模板列表，扩展名与当前输出目标匹配的模板将被自动选择，例如—

@code{◊(define-meta template (list "my-template.html" "my-template.txt" "my-template.pdf"))}




}

@item{如果此键不存在，或引用不存在的文件，则查找名称为 @code[(format "~a.[output extension]" pollen-template-prefix)] 的默认模板。意思是，如果 @racket[_source-path] 是@code[(format "intro.html.~a" pollen-markup-source-ext)]，那么输出路径就是 @code["intro.html"] ，所以默认模板是@code[(format "~a.html" pollen-template-prefix)]。在与源文件相同的目录中查找此默认模板，然后在连续的父目录中向上搜索。 （推论：项目根目录中的默认模板将应用于项目中的所有文件，除非在子目录中被覆盖。）}

@item{I如果此文件不存在，请使用后备模板作为最后的手段。 （另见 @secref["Templates"
         #:tag-prefixes '("tutorial-2")
         #:doc '(lib "pollen/scribblings/pollen.scrbl")].)}
]

当需要一个模板，但缺少 @racket[_template-path] 参数时（例如，在 @racket[render] 或 @racket[render-to-file] 中），会调用这个函数。