/*
    Term to Html converter. 

    use node_html(Nodes, Html) to convert the prolog structure to an HTML string.

    Each element type can take children and some can take attributes. 
    Text content must be wrapped in an s(_) term to identify it as different to other content.
    Examples; 
    1. A paragraph with some text.

    p(s("This is the text")).

    2. A div with class and two paragraphs.

    div([class("my-div")], [
        p(s("This is the first paragraph.")),
        p(s("This is the second paragraph."))
    ]).

    Not every attribute type or HTML type has been mapped, but they are easy to map below. 
    - el_type(ElementTerm, Definition) maps the elements, there are plenty of examples.
    - el_attribute(AttributeTerm, StringName, Value) maps attribute types. 
*/

% 
% HTML Elements 
%
el_unify(A,A).

% Section Elements
el_type(html(C), node("html", C)).
el_type(body(C), node("body", C)).
el_type(head(C), node("head", C)).
el_type(header(C), node("header", C)).
el_type(main(C), node("main", C)).
el_type(footer(C), node("footer", C)).
el_type(nav(C), node("nav", C)).
el_type(article(C), node("article", C)).
el_type(section(C), node("section", C)).
el_type(section(A, C), attr("section", C, A)).

% Format elements
el_type(div(C), node("div", C)).
el_type(div(A, C), node("div", C, A)).
el_type(span(C), node("span", C)).
el_type(span(A, C), node("span", C, A)).
el_type(hr, attr_only("hr", [])).
el_type(br, attr_only("br", [])).
el_type(p(C), node("p", C)).
el_type(pre(C), node("pre", C)).
el_type(code(C), node("code", C)).
el_type(blockquote(C), node("blockquote", C)).

% Header elements
el_type(title(C), node("title", C)).
el_type(script(C), node("script", C)).
el_type(script(A, C), attr("script", C, A)).
el_type(style(C), node("style", C)).
el_type(link(A), attr_only("link",  A)).
el_type(meta(A), attr_only("meta", A)).

% Text elements
el_type(h1(C), node("h1", C)).
el_type(h2(C), node("h2", C)).
el_type(h3(C), node("h3", C)).
el_type(a(A,C), attr("a", C, A)).

% List elements
el_type(ol(C), node("ol", C)).
el_type(ul(C), node("ul", C)).
el_type(li(C), node("li", C)).
el_type(dl(C), node("dl", C)).
el_type(dt(C), node("dt", C)).
el_type(dd(C), node("dd", C)).

% String types
el_type(s(C), any(C)).

% Valid HTML Attributes
el_attribute(href(V),    "href",    V).
el_attribute(class(V),   "class",   V).
el_attribute(rel(V),     "rel",     V).
el_attribute(title(V),   "title",   V).
el_attribute(id(V),      "id",      V).
el_attribute(name(V),    "name",    V).
el_attribute(content(V), "content", V).

%
% Node to HTML predicates.
%

% Convert a node tree to a HTML string.
node_html(RootNode, Html) :-
    el(RootNode, Html, []).

% An HTML elememt
el(Node, A, B) :- 
    el_type(Node, Type),
    el_typed(Type, A, B).

% An element categorised by type.
el_typed(node(Text, C), A, B) :- 
    el_node(Text, [], C, A, B).
el_typed(attr(Text, C, Attr), A, B) :-
    el_node(Text, Attr, C, A, B).
el_typed(attr_only(Text, Attr), A, B) :-
    el_attr_only_node(Text, Attr, A, B).
el_typed(any(C), A, B) :- 
    el_any(C, A, B).

% Generate a single HTML element node.
el_node(Node, Attributes, Children, [<|A], B) :-
    el_any(Node, A, C),
    el_attributes(Attributes, C, D),
    el_unify(D, [>|E]),
    el_children(Children, E, F),
    el_unify(F, [<, /|G]),
    el_any(Node, G, H),
    el_unify(H, [>|B]).

el_attr_only_node(Node, Attributes, [<|A], B) :-
    el_any(Node, A, C),
    el_attributes(Attributes, C, D),
    el_unify(D, [' ',/,>|B]).
 
% Write any attributes to the HTML node.
el_attributes([], A, A).
el_attributes([Att|Next], A, B) :-
    el_unify(A, [' '|C]),
    el_attribute(Att, AttName, Value),
    el_any(AttName, C, D),
    el_unify(D, ['="'|E]),
    el_any(Value, E, F),
    el_unify(F, ['"'|G]),
    el_attributes(Next, G, B).

% Generate the children for an HTML node.
el_children(s(S), A, B) :- el_any(S, A, B).
el_children([], A, A).
el_children([El|Next], A, B) :-
    el(El, A, C),
    el_children(Next, C, B).

% Write the contents to the output directly
el_any([], A, A).
el_any([C|T], [C|A], B) :-
    el_any(T, A, B).
