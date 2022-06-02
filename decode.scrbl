#lang scribble/manual

@(require "mb-tools.rkt" scribble/eval pollen/decode pollen/setup txexpr racket/string (for-label racket(except-in pollen #%module-begin) pollen/setup pollen/cache pollen/decode txexpr xml))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode xml racket/list txexpr))


@title{Decode}

@defmodule[pollen/decode]

 Pollen 标记文件的 @racket[doc] 输出是一个简单的 X-表达式。 @italic{Decoding} 指的是对这个 X 表达式的任何后处理。@racket[pollen/decode] 模块提供了创建解码器的工具。

解码步骤可以与文件的编译分开进行。但是您也可以将解码器附加到标记文件的@racket[root] 节点，因此在编译标记时会自动进行解码，从而自动合并到@racket[doc] 中。 （按照这种方法，您还可以将多个解码器附加到 @racket[doc] 中的不同标签。）

当然，您可以在 Pollen 标记中嵌入函数调用。但是由于标记针对作者进行了优化，因此解码对于可以或应该移出创作层的操作是很有用的。

一个例子是演示和布局。例如， @racket[decode-paragraphs] 是一个解码器函数，可以让作者在他们的源代码中简单地使用两个回车键来标记段落。

另一个例子是将输出转换为一种特定的数据格式。大多数 Pollen 函数都针对 HTML 输出进行了优化，但人们可以编写一个针对其他格式的解码器。



@defproc[
(decode
[tagged-xexpr txexpr?]
[#:txexpr-tag-proc txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?) (λ (tag) tag)]
[#:txexpr-attrs-proc txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?) (λ (attrs) attrs)]
[#:txexpr-elements-proc txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?) (λ (elements) elements)]
[#:txexpr-proc txexpr-proc (txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:block-txexpr-proc block-txexpr-proc (block-txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:inline-txexpr-proc inline-txexpr-proc (txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:string-proc string-proc (string? . -> . (or/c xexpr? (listof xexpr?))) (λ (str) str)]
[#:entity-proc entity-proc ((or/c symbol? valid-char?) . -> . (or/c xexpr? (listof xexpr?))) (λ (ent) ent)]
[#:cdata-proc cdata-proc (cdata? . -> . (or/c xexpr? (listof xexpr?))) (λ (cdata) cdata)]
[#:exclude-tags tags-to-exclude (listof txexpr-tag?) null]
[#:exclude-attrs attrs-to-exclude txexpr-attrs? null]
)
(or/c xexpr/c (listof xexpr/c))]
递归处理的 @racket[_tagged-xexpr] ，通常是从 Pollen 源文件导出的 @racket[doc] 。

这个函数本身并没有做太多的事情。相反，它提供了可以可以挂上更多的工作函数的钩子。

回顾一下，在 Pollen 中，所有的 @secref["tag-are-functions"] 。默认情况下，来自源文件的 @racket[_tagged-xexpr] 被标记为 @racket[root] 。所以使用 @racket[decode] 的典型方法是将你的解码函数附加到它上面，然后定义 @racket[root] 来调用你的 @racket[decode] 函数。然后在编译过程中，它将自动应用于每一个 @racket[doc] 。

@margin-note{@link["https://docs.racket-lang.org/pollen-tfl/_pollen_rkt_.html#%28def._%28%28lib._pollen-tfl%2Fpollen..rkt%29._root%29%29"]{这里有一个通过 @racket[root] 标签调用 @racket[decode] 的例子}。这个例子是 @racket[pollen-tfl] 样本项目的一部分，你可以单独安装和研究。}

这说明了另一个重要点：即使 @racket[decode] 提供了一个强大的参数列表，您也不太可能一次使用所有这些参数。这些代表可能性，而不是要求。例如，让我们看看在没有任何可选参数的情况下调用 @racket[decode] 会发生什么。

@examples[#:eval my-eval
(define tx '(root "I wonder" (em "why") "this works."))
(decode tx)
]

对--什么都没有。这是因为解码参数的默认值是身份函数，@racket[(λ (x)x)]。所以所有的输入都会被完整地传递过去，除非指定了另一个操作。

@racket[decode] 的@racket[_*-proc] 参数采用适用于@racket[_txexpr] 中特定类别元素的过程。

@racket[_txexpr-tag-proc]参数是一个处理 X-表达式标签的过程。

@examples[#:eval my-eval
(define tx '(p "I'm from a strange" (strong "namespace")))
(code:comment @#,t{Tags are symbols, so a tag-proc should return a symbol})
(decode tx #:txexpr-tag-proc (λ (t) (string->symbol (format "ns:~a" t))))
]

@racket[_txexpr-attrs-proc] 参数是一个处理 X 表达式属性列表的过程。 （@racketmodname[txexpr] 模块免费包含在 Pollen 中，包括用于处理这些属性列表的有用帮助函数。）

@examples[#:eval my-eval
(define tx '(p ((id "first")) "If I only had a brain."))
(code:comment @#,t{Attrs is a list, so cons is OK for simple cases})
(decode tx #:txexpr-attrs-proc (λ (attrs) (cons '[class "PhD"] attrs )))
]

请注意，@racket[_txexpr-attrs-proc] 将更改每个标记的 X 表达式的属性，即使是那些没有属性的。这很有用，因为有时您想添加以前不存在的属性。但要小心，因为该行为可能会使您的处理函数过度包容。

@examples[#:eval my-eval
(define tx '(div (p ((id "first")) "If I only had a brain.") 
(p "Me too.")))
(code:comment @#,t{This will insert the new attribute everywhere})
(decode tx #:txexpr-attrs-proc (λ (attrs) (cons '[class "PhD"] attrs )))
(code:comment @#,t{This will add the new attribute only to non-null attribute lists})
(decode tx #:txexpr-attrs-proc 
(λ (attrs) (if (null? attrs) attrs (cons '[class "PhD"] attrs ))))
]


@racket[_txexpr-elements-proc] 参数是一个对元素列表进行操作的过程，该列表表示每个标记的 X 表达式的内容。请注意，X 表达式的每个元素都要经过两次解码器：一次是现在，作为元素列表的成员，另一次是以后，通过其特定类型的解码器（即 @racket[_string-proc] , @racket[_entity-proc] ，等等）。

@examples[#:eval my-eval
(define tx '(div "Double" "\n" "toil" amp "trouble")) 
(code:comment @#,t{Every element gets doubled ...})
(decode tx #:txexpr-elements-proc (λ (es) (append-map (λ (e) (list e e)) es)))
(code:comment @#,t{... but only strings get capitalized})
(decode tx #:txexpr-elements-proc (λ (es) (append-map (λ (e) (list e e)) es))
#:string-proc (λ (s) (string-upcase s)))
]

那么为什么需要 @racket[_txexpr-elements-proc] ？因为有些类型的元素解码取决于上下文，因此有必要将元素作为一个组来处理。例如，段落解码。这种行为不仅仅是跨越每个元素的 @racket[map] ，因为元素正在被删除并根据上下文进行更改：

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc decode-paragraphs))
(code:comment @#,t{Context matters. Trailing whitespace is ignored ...})
(paras '(body "The first paragraph." "\n\n")) 
(code:comment @#,t{... but whitespace between strings is converted to a break.})
(paras '(body "The first paragraph." "\n\n" "And another.")) 
(code:comment @#,t{A combination of both types})
(paras '(body "The first paragraph." "\n\n" "And another." "\n\n")) 
]


@racket[_txexpr-proc], @racket[_block-txexpr-proc] , 和 @racket[_inline-txexpr-proc] 参数是对标记的 X 表达式进行操作的过程。如果 X 表达式满足 @racket[block-txexpr?] 测试，它将被 @racket[_block-txexpr-proc] 处理。否则，它是内联的，所以它被 @racket[_inline-txexpr-proc] 处理。(但是要注意--这些并不是相互排斥的，因为 @racket[_block-txexpr-proc] 对一个块的所有元素进行操作，包括其中的其他标记的 X 表达式)。然后两个类别都由 @racket[_txexpr-proc] 来处理。

@examples[#:eval my-eval
(define tx '(div "Please" (em "mind the gap") (h1 "Tuesdays only"))) 
(define add-ns (λ (tx) (txexpr 
    (string->symbol (format "ns:~a" (get-tag tx))) 
    (get-attrs tx) 
    (get-elements tx))))
(code:comment @#,t{div and h1 are block elements, so this will only affect them})
(decode tx #:block-txexpr-proc add-ns)
(code:comment @#,t{em is an inline element, so this will only affect it})
(decode tx #:inline-txexpr-proc add-ns)
(code:comment @#,t{this will affect all elements})
(decode tx #:block-txexpr-proc add-ns #:inline-txexpr-proc add-ns)
(code:comment @#,t{as will this})
(decode tx #:txexpr-proc add-ns)
]

@racket[_string-proc], @racket[_entity-proc] , 和 @racket[_cdata-proc] 参数是分别对字符串、实体和 CDATA 的 X 表达式的操作过程。有意的，这些程序的输出合约接受任何种类的 X-表达式（意味着，程序可以改变 X-表达式的类型）。

@examples[#:eval my-eval
(code:comment @#,t{A div with string, entity, and cdata elements})
(define tx `(div "Moe" amp 62 ,(cdata #f #f "3 > 2;")))
(define rulify (λ (x) '(hr)))
(code:comment @#,t{The rulify function is selectively applied to each})
(print (decode tx #:string-proc rulify))
(print (decode tx #:entity-proc rulify))
(print (decode tx #:cdata-proc rulify))
] 

注意，实体有两种类型--符号型和数字型--而 @racket[_entity-proc] 对这两种类型都有影响。如果你只想影响其中之一，你可以在 @racket[_entity-proc] 中添加一个测试。符号实体可以用 @racket[symbol?] 解码，数字实体可以用 @racket[valid-char?] 解码：

@examples[#:eval my-eval
(define tx `(div amp 62))
(define symbolic-detonate (λ (x) (if (symbol? x) 'BOOM x)))
(print (decode tx #:entity-proc symbolic-detonate))
(define numeric-detonate (λ (x) (if (valid-char? x) 'BOOM x)))
(print (decode tx #:entity-proc numeric-detonate))
] 

之前的五个过程-- @racket[_block-txexpr-proc] , @racket[_inline-txexpr-proc] , @racket[_string-proc] , @racket[_entity-proc] , 和 @racket[_cdata-proc] --可以返回一个单一的 X 表达式，或者一个 X 表达式的列表，它们将在同一个点被拼接到父级。

例如，前面我们看到了如何使用 @racket[_txexpr-elements-proc] 将元素加倍。但您可以通过返回一系列值来逐个完成相同的工作：

@examples[#:eval my-eval
(code:comment @#,t{A div with string, entity, and inline-txexpr elements})
(define tx `(div "Axl" amp (span "Slash")))
(define doubler (λ (x) (list x x)))
(code:comment @#,t{The doubler function is selectively applied to each type of element})
(print (decode tx #:string-proc doubler))
(print (decode tx #:entity-proc doubler))
(print (decode tx #:inline-txexpr-proc doubler))
] 

注意：当返回列表值时，可能会在 @racket[txexpr?] 和恰好以符号实体开头的 @racket[xexpr?] 列表之间出现不可避免的歧义：

@examples[#:eval my-eval
(code:comment @#,t{An ambiguous expression})
(define amb '(guitar "player-name"))
(and (txexpr-elements? amb) (txexpr? amb))
(code:comment @#,t{Ambiguity in context})
(define x '(gnr "Izzy" "Slash"))
(define rockit (λ (str) (list 'guitar str)))
(code:comment @#,t{Expecting '(gnr guitar "Izzy" guitar "Slash") from next line,
but return value will be treated as tagged X-expression})
(decode x #:string-proc rockit)
(code:comment @#,t{Changing the order makes it unambiguous})
(define rockit2 (λ (str) (list str 'guitar)))
(decode x #:string-proc rockit2)
] 

@racket[_tags-to-exclude]参数是一个可以免于解码的标记列表。虽然您可以通过在各个解码函数中测试输入来获得相同的结果，但这很乏味，而且可能会很慢。

@examples[#:eval my-eval
(define tx '(p "I really think" (em "italics") "should be lowercase."))
(decode tx #:string-proc string-upcase)
(decode tx #:string-proc string-upcase #:exclude-tags '(em))
]

如果你要对注定要成为 HTML 的源文件进行解码，那么 @racket[_tags-to-exclude] 参数就很有用。根据 HTML 规范， @racket[<style>] 或 @racket[<script>] 块内的材料需要保留字面意思。在这个例子中，如果 CSS 和 JavaScript 块被大写了，它们就不能工作。所以排除了@racket['(style script)]，问题就解决了。

@examples[#:eval my-eval
(define tx '(body (h1 ((class "Red")) "Let's visit Planet Telex.") 
(style ((type "text/css")) ".Red {color: green;}")
(script ((type "text/javascript")) "var area = h * w;")))
(decode tx #:string-proc string-upcase)
(decode tx #:string-proc string-upcase #:exclude-tags '(style script))
]

最后， @racket[_attrs-to-exclude] 参数的工作方式与 @racket[_tags-to-exclude] 相同，但它不是基于标记排除元素，而是基于元素是否具有匹配的属性/值对进行排除。

@examples[#:eval my-eval
(define tx '(p (span "No attrs") (span ((id "foo")) "One attr")))
(decode tx #:string-proc string-upcase)
(decode tx #:string-proc string-upcase #:exclude-attrs '((id "foo")))
]

@defproc[
(decode-elements
[elements txexpr-elements?]
[#:txexpr-tag-proc txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?) (λ (tag) tag)]
[#:txexpr-attrs-proc txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?) (λ (attrs) attrs)]
[#:txexpr-elements-proc txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?) (λ (elements) elements)]
[#:txexpr-proc txexpr-proc (txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:block-txexpr-proc block-txexpr-proc (block-txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:inline-txexpr-proc inline-txexpr-proc (txexpr? . -> . (or/c xexpr? (listof xexpr?))) (λ (tx) tx)]
[#:string-proc string-proc (string? . -> . (or/c xexpr? (listof xexpr?))) (λ (str) str)]
[#:entity-proc entity-proc ((or/c symbol? valid-char?) . -> . (or/c xexpr? (listof xexpr?))) (λ (ent) ent)]
[#:cdata-proc cdata-proc (cdata? . -> . (or/c xexpr? (listof xexpr?))) (λ (cdata) cdata)]
[#:exclude-tags tags-to-exclude (listof txexpr-tag?) null]
[#:exclude-attrs attrs-to-exclude txexpr-attrs? null]
)
(or/c xexpr/c (listof xexpr/c))]
与 @racket[decode] 相同，但需要 @racket[txexpr-elements?] 作为输入，而不是整个标记的 X 表达式。一个方便的变体，在标签函数中使用。


@defproc[
(block-txexpr?
[v any/c])
boolean?]
检验 @racket[_v] 是否有属于 @racket[setup:block-tags] 的标签的谓词。如果不是，它将被视为内联。

该谓词影响其他函数的行为。例如，@racket[decode-paragraphs] 知道标记中的块元素不应该包含在 @racket[p] 标记中。因此，如果您引入一个名为 @racket[bloq] 的新块元素而不将其配置为块，则会出现错误行为：

@examples[#:eval my-eval
(define (paras tx) (decode tx #:txexpr-elements-proc decode-paragraphs))
(paras '(body "I want to be a paragraph." "\n\n" (bloq "But not me."))) 
(code:comment @#,t{Wrong: bloq should not be wrapped})
]

要改变这个测试的工作方式，请使用 @secref["setup-overrides"] 中描述的 @racket[setup] 子模块。

@racketblock[
(module setup racket/base
  (provide (all-defined-out))
  (require pollen/setup)
  (define block-tags (cons 'bloq default-block-tags)))]

更改后，结果将是：

@racketresultfont{'(body (p "I want to be a paragraph.") (bloq "But not me."))}

默认块标签是：

@racketidfont{@(string-join (map symbol->string default-block-tags) " ")}


@defproc[
(merge-newlines
[elements (listof xexpr?)])
(listof xexpr?)]
在 @racket[_elements] 中，将连续的换行字符合并为一个元素。换行字符串由 @racket[setup:newline] 控制，并默认为 @val[default-newline] 。

@examples[#:eval my-eval
(merge-newlines '(p "\n" "\n" "foo" "\n" "\n\n" "bar" 
  (em "\n" "\n" "\n")))]


@defproc[
(decode-linebreaks
[elements (listof xexpr?)]
[linebreaker (or/c #f xexpr? (xexpr? xexpr? . -> . (or/c #f xexpr?))) '(br)])
(listof xexpr?)]
在@racket[_elements] 中，将出现的换行符分隔符转换为 @racket[_linebreaker] ，但前提是分隔符不在块之间出现（参见 @racket[block-txexpr?] ）。为什么？因为块级元素会自动显示在新行上，所以添加 @racket[_linebreaker] 将是多余的。在这种情况下，换行符就会消失。

换行符由@racket[setup:linebreak-separator] 控制，默认为 @val[default-linebreak-separator] 。

@racket[_linebreaker] 参数可以是 @racket[#f] （将删除换行符）、X 表达式（将替换换行符）或接受两个 X 表达式并返回一个的函数。此函数将接收前一个和下一个元素，以使上下文替换成为可能。

@examples[#:eval my-eval
(decode-linebreaks '(div "Two items:" "\n" (em "Eggs") "\n" (em "Bacon")))
(decode-linebreaks '(div "Two items:" "\n" (em "Eggs") "\n" (em "Bacon")) #f)
(decode-linebreaks '(div "Two items:" "\n" (div "Eggs") "\n" (div "Bacon")))
(decode-linebreaks '(div "Two items:" "\n" (em "Eggs") "\n" (em "Bacon"))
 (λ (prev next) (if (and (txexpr? prev) (member "Eggs" prev)) '(egg-br) '(br))))
]

@defproc[
(decode-paragraphs
[elements (listof xexpr?)]
[paragraph-wrapper (or/c txexpr-tag? ((listof xexpr?) . -> . txexpr?)) 'p]
[#:linebreak-proc linebreak-proc (txexpr-elements? . -> . txexpr-elements?) decode-linebreaks]
[#:force? force-paragraph? boolean? #f])
txexpr-elements?]
在 @racket[_elements] 中寻找段落，并用 @racket[_paragraph-wrapper] 来包装它们。还可以使用 @racket[dele-lineebreaks] 来处理行距。

什么算作一个段落？任何 @racket[_elements] ，要么 a)明确地用段落分隔符分开，要么 b)与 @racket[block-txexpr?] 相邻（在这种情况下，段落性是隐含的）。

段落分隔符由 @racket[setup:paragraph-separator] 控制，并默认为 @val[default-paragraph-separator] 。

@examples[#:eval my-eval
(decode-paragraphs '("Explicit para" "\n\n" "Explicit para"))
(decode-paragraphs '("Explicit para" "\n\n" "Explicit para" "\n" "Explicit line"))
(decode-paragraphs '("Implied para" (div "Block") "Implied para"))
]

如果 @racket[_element] 已经是一个块，它将不会被包装成一个段落（因为在这种情况下，包装将是多余的）。因此，如果 @racket[_paragraph-sep] 出现在两个块之间，它将被忽略（如下面的例子中使用两个连续的 @racket[div] 块）。

@examples[#:eval my-eval
(code:comment @#,t{The explicit "\n\n" makes no difference in these cases})
(decode-paragraphs '((div "First block") "\n\n" (div "Second block")))
(decode-paragraphs '((div "First block") (div "Second block")))
(decode-paragraphs '("Para" "\n\n" (div "Block")))
(decode-paragraphs '("Para" (div "Block")))
]

@racket[_paragraph-wrapper]参数可以是一个 X-表达式，也可以是一个函数，它接收一个元素列表并返回一个标记的 X-表达式。这个函数将接收段落的元素，以使上下文包装成为可能。

@examples[#:eval my-eval
(decode-paragraphs '("First para" "\n\n" "Second para") 'ns:p)
(decode-paragraphs '("First para" "\n\n" "Second para") 
 (λ (elems) `(ns:p ,@elems "!?!")))
]

@racket[_linebreak-proc]参数允许你使用不同的断行程序，而不是通常的 @racket[decode-linebreaks] 。

@examples[#:eval my-eval
(decode-paragraphs '("First para" "\n\n" "Second para" "\n" "Second line")
#:linebreak-proc (λ (x) (decode-linebreaks x '(newline))))
]

@racket[#:force?] 选项将在 @racket[_elements] 周围包裹一个段落标签，即使没有发现显性或隐性的段落分隔符。@racket[#:force?] 选项在你想保证总是得到一个块的列表时很有用。

@examples[#:eval my-eval
(decode-paragraphs '("This" (span "will not be") "a paragraph"))
(decode-paragraphs '("But this" (span "will be") "a paragraph") #:force? #t)
]


