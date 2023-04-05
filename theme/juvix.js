/*
The following is specifically tailored for compatibility with HighlightJS
10.1.1, which is used in MdBook. Support for the latest version of HighlightJS,
v11.7, can be found at the following repository:
https://github.com/anoma/highlightjs-juvix
*/

var module = module ? module : {}; // shim for browser use

const MATCH_NOTHING_RE = /$^/; // to force a mode to never match

const INLINE_COMMENT = {
  className: "comment",
  begin: /--/,
  end: /$/,
};

const BLOCK_COMMENT = {
  className: "comment",
  begin: /\{-/,
  end: /-\}/,
  relevance: 1,
};

const COMMENT = {
  scope: "comment",
  contains: ["self"],
  variants: [INLINE_COMMENT, BLOCK_COMMENT],
};

const STRINGS = {
  className: "string",
  variants: [hljs.QUOTE_STRING_MODE],
};

const NUMBERS = hljs.C_NUMBER_MODE;

const RESERVED_SYMBOLS = {
  className: "keyword",
  begin:
    /(@|:=|->|→|↦|;|\||\{|\}|\\|λ|\s:\s|\_|Type|\slet\s|\sin\s|terminating|positive|axiom|builtin|open|end)/,
  endsSameBegin: true,
};

const OTHER_SYMBOLS_AND_OPERATORS = [
  {
    className: "operator",
    begin: /(\*|\|\||&&|==|>>|=?>|<=?|-\s|\+)/,
    endsSameBegin: true,
  },
  { className: "punctuation", begin: /(\(|\))/, endsSameBegin: true },
  { begin: /::/, endsSameBegin: true },
  // an issue of this hljs version is that some keywords are not highlighted
  // even though they are in the keywords list. This is a workaround
  { className: "literal", begin: /(true|false)/, endsSameBegin: true },
  { className: "built_in", begin: /(trace|IO|if|case)/, endsSameBegin: true },
];

const MODULE_NAME_RE = /[a-zA-Z][a-zA-Z0-9_]*(\.[a-zA-Z][a-zA-Z0-9_]*)*/;
const IDEN_RE = /[^(\s|;|\{|\}|\(|\)|@)]+/;
//some symbols may brake the use of this

function hljsDefineJuvix(hljs) {
  const JUVIX_KEYWORDS = {
    keyword: "let in print terminating positive axiom builtin open end",
    built_in: "trace IO if case",
    literal: "true false",
  };

  const MODULE_BEGIN = {
    className: "keyword",
    begin: /module/,
    end: /;/,
    contains: [
      BLOCK_COMMENT,
      {
        className: "title",
        begin: MODULE_NAME_RE,
        endsParent: true,
        contains: [BLOCK_COMMENT],
      },
    ],
  };

  const PUBLIC = {
    className: "keyword",
    begin: /public/,
    end: /;/,
    endsParent: true,
    contains: [COMMENT],
  };

  const MODULE_END = {
    className: "keyword",
    begin: /end/,
    end: /;/,
    contains: [COMMENT],
  };

  const INFIX = {
    className: "keyword",
    begin: /((infix(l|r)?)|postfix)/,
    end: MATCH_NOTHING_RE,
    contains: [
      COMMENT,
      {
        className: "number",
        begin: /\d+/,
        end: MATCH_NOTHING_RE,
        endsParent: true,
        contains: [
          COMMENT,
          {
            className: "operator",
            begin: IDEN_RE,
            end: MATCH_NOTHING_RE,
            endsParent: true,
            contains: [
              COMMENT,
              {
                className: "keyword",
                begin: /;/,
                endsSameBegin: true,
                endsParent: true,
              },
            ],
          },
        ],
      },
    ],
  };

  const IMPORT = {
    className: "keyword",
    begin: /(((open )?import)|(open( import)?))/,
    end: MATCH_NOTHING_RE,
    // endsParent: true,
    contains: [
      COMMENT,
      {
        className: "title",
        begin: MODULE_NAME_RE,
        end: MATCH_NOTHING_RE,
        endsParent: true,
        contains: [
          COMMENT,
          PUBLIC,
          {
            className: "keyword",
            begin: /;/,
            endsParent: true,
          },
          {
            className: "keyword",
            begin: /(hiding|using)/,
            end: MATCH_NOTHING_RE,
            contains: [
              COMMENT,
              {
                className: "keyword",
                begin: /\{/,
                end: /\}/,
                endsParent: true,
                contains: [
                  COMMENT,
                  {
                    className: "symbol",
                    begin: IDEN_RE,
                    contains: [COMMENT, PUBLIC],
                  },
                ],
              },
            ],
          },
        ],
      },
    ],
  };

  const INDUCTIVE = {
    className: "inductive",
    begin: /type/,
    end: MATCH_NOTHING_RE,
    keywords: "type",
    contains: [
      COMMENT,
      {
        className: "keyword",
        begin: /;/,
        endsParent: true,
      },
      {
        // className: "symbol",
        begin: IDEN_RE,
        end: MATCH_NOTHING_RE,
        endsParent: true,
        contains: [
          COMMENT,
          RESERVED_SYMBOLS,
          {
            //  ( A : Type )
            begin: /\(/,
            end: /\)/,
            contains: [
              COMMENT,
              {
                // className: "symbol",
                begin: IDEN_RE,
                end: MATCH_NOTHING_RE,
                endsParent: true,
                contains: [
                  COMMENT,
                  {
                    // type annotation
                    className: "keyword",
                    begin: /:/,
                    end: /Type/,
                    endsParent: true,
                  },
                ],
              },
            ],
          },
        ],
      },
    ],
  };

  var OPTS = [
    COMMENT,
    RESERVED_SYMBOLS,
    MODULE_BEGIN,
    MODULE_END,
    IMPORT,
    INFIX,
    INDUCTIVE,
    NUMBERS,
    STRINGS,
  ];

  return {
    name: "Juvix",
    aliases: ["juvix"],
    keywords: JUVIX_KEYWORDS,
    contains: OPTS.concat({
      begin: /(?=[^(\s|;|\{|\}|\(|\)|@)]+\s*:)/,
      end: MATCH_NOTHING_RE,
      endsSameBegin: true,
      contains: [
        {
          className: "function",
          begin: /^\s*[^(\s|;|\{|\}|\(|\)|@)]+(?=\s*:\s+.*;)/,
          end: MATCH_NOTHING_RE,
          contains: [
            COMMENT,
            {
              className: "keyword",
              begin: /:\s+/,
              endsSameBegin: true,
              endsParent: true,
            },
            {
              className: "keyword",
              begin: /;/,
              endsSameBegin: true,
              endsParent: true,
            },
          ],
        },
        {
          className: "keyword",
          begin: /;/,
          endsSameBegin: true,
          endsParent: true,
        },
      ]
        .concat(OPTS)
        .concat([OTHER_SYMBOLS_AND_OPERATORS]),
    }),
  };
}

function hljsDefineJuvixRepl(hljs) {
  return {
    name: "Juvix REPL",
    aliases: ["jrepl"],
    case_insensitive: false,
    unicodeRegex: true,
    contains: [
      {
        className: "meta",
        begin: /^(Juvix REPL|OK)/,
        end: /$/,
      },
      {
        begin: /^(?=([a-zA-Z][a-zA-Z0-9_]*(\.[a-zA-Z][a-zA-Z0-9_]*))?>)/,
        end: /$/,
        contains: [
          {
            className: "meta",
            begin: /([a-zA-Z][a-zA-Z0-9_]*(\.[a-zA-Z][a-zA-Z0-9_]*))?>\s+/,
            endsSameBegin: true,
          },
          {
            className: "keyword",
            begin: /:[a-z]+/,
            endsWithParent: true,
            contains: [
              {
                subLanguage: "juvix",
                endsWithParent: true,
              },
            ],
          },
          {
            subLanguage: "juvix",
            endsWithParent: true,
          },
        ],
      },
      {
        begin: /^/,
        end: /$/,
        contains: [
          {
            subLanguage: "juvix",
            endsWithParent: true,
          },
        ],
      },
    ],
  };
}

module.exports = function (hljs) {
  hljs.registerLanguage("juvix", hljsDefineJuvix);
  hljs.registerLanguage("jrepl", hljsDefineJuvixRepl);
};
