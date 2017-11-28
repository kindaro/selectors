{ module Text.XML.Selectors.CSS.Parse (parsePath) where

import Text.XML.Selectors.CSS.Tokens (lexer)
import Text.XML.Selectors.CSS.Types
}

%name cssPath
%tokentype { Token }
%error { parseError }
%monad { Either String }
%token
    sp                      { TokenSpace }
    name                    { TokenName $$ }
    string                  { TokenString $$ }
    '+'                     { TokenPlus }
    '-'                     { TokenMinus }
    '/'                     { TokenSlash }
    '>'                     { TokenChild }
    '~'                     { TokenAnySibling }
    '#'                     { TokenHash }
    firstchild              { TokenFirstChild }
    lastchild               { TokenLastChild }
    '.'                     { TokenDot }
    '='                     { TokenEquals }
    "~="                    { TokenIncludes }
    "^="                    { TokenBeginsWith }
    "$="                    { TokenEndsWith }
    "|="                    { TokenDashMatch }
    ':'                     { TokenPseudo }
    '('                     { TokenOP }
    ')'                     { TokenCP }
    '['                     { TokenOB }
    ']'                     { TokenCB }
    nat                     { TokenDigits $$ }
    '*'                     { TokenAster }
    nthchild                { TokenNthChild }

%%

Selector    : SimpleSelector                        { Selector $1 }
            | SimpleSelector Combinator Selector    { Combinator $1 $2 $3 }

Combinator  : sp                                    { Descendant }
            | sp Combinator                         { $2 }
            | '+'                                   { FollowingSibling }
            | '+' sp                                { FollowingSibling }
            | '~'                                   { AnySibling }
            | '~' sp                                { AnySibling }
            | '>'                                   { Child }
            | '>' sp                                { Child }

SimpleSelector  : name                              { SimpleSelector (Just $1) [] Nothing }
                | name specs                        { SimpleSelector (Just $1) $2 Nothing }
                | name specs Pseudo                 { SimpleSelector (Just $1) $2 (Just $3) }
                | name Pseudo                       { SimpleSelector (Just $1) [] (Just $2) }
                | '*' specs                         { SimpleSelector Nothing $2 Nothing }
                | '*' specs Pseudo                  { SimpleSelector Nothing $2 (Just $3) }
                | '*' Pseudo                        { SimpleSelector Nothing [] (Just $2) }
                | '*'                               { SimpleSelector Nothing [] Nothing }
                -- These ones are not specified but I believe are nonetheless in common usage.
                | specs                             { SimpleSelector Nothing $1 Nothing }
                | specs Pseudo                      { SimpleSelector Nothing $1 (Just $2) }
                | Pseudo                            { SimpleSelector Nothing [] (Just $1) }

specs   : Specifier                                 { [$1] }
        | specs Specifier                           { $2 : $1 }

Specifier   : '#' name                              { ID $2 }
            | '.' name                              { Class $2 }
            | '[' attr ']'                          { $2 }
            | '[' attr sp ']'                       { $2 }

attr    : name Pred                                 { Attrib $1 $2 }
        | sp attr                                   { $2 }

Pred    : sp Pred                                   { $2 }
        | PredOp sp string                          { Pred $1 $3 }
        | PredOp string                             { Pred $1 $2 }

PredOp  : '='                                       { Equals }
        | "~="                                      { Includes }
        | "|="                                      { DashMatch }
        | "^="                                      { BeginsWith }
        | "$="                                      { EndsWith }

Pseudo  : ':' firstchild                            { FirstChild }
        | ':' lastchild                             { LastChild }
        | ':' nthchild '(' nat ')'                  { NthChild (read $4) }

{
parseError toks = Left $ "parse error: " ++ show toks

parsePath :: String -> Either String Selector
parsePath str = lexer str >>= cssPath
}
