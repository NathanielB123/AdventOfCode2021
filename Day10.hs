{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Data.List (sort)

day10p1 :: [String] -> Int
day10p1 ls 
  = sum $ map (`day10'` []) ls
  where
    day10' :: [Char] -> [Char] -> Int
    day10' (x: xs) p@(ph: pt)
      | x == '(' = day10' xs $ ')' : p
      | x == '[' = day10' xs $ ']' : p
      | x == '{' = day10' xs $ '}' : p
      | x == '<' = day10' xs $ '>' : p
      | x == ph = day10' xs pt
      | x == ')' = 3
      | x == ']' = 57
      | x == '}' = 1197
      | x == '>' = 25137
      | otherwise = error "Unexpected character encountered"
    day10' [] _
      = 0
    day10' xs []
      = day10' xs ['E']

day10p2 :: [String] -> Int
day10p2 ls 
  = vs !! (length vs `div` 2)
  where
    vs = sort $ filter (/= 0) $ map (\l -> day10' (l ++ "E") []) ls
    day10' :: [Char] -> [Char] -> Int
    day10' (x: xs) p@(ph: pt)
      | x == '(' = day10' xs $ ')' : p
      | x == '[' = day10' xs $ ']' : p
      | x == '{' = day10' xs $ '}' : p
      | x == '<' = day10' xs $ '>' : p
      | x == ph = day10' xs pt
      | x == ')' = 0
      | x == ']' = 0
      | x == '}' = 0
      | x == '>' = 0
      | x == 'E' = scoreRem $ reverse p
      | otherwise = error "Unexpected character encountered"
    day10' [] _
      = error "Should not be empty"
    day10' xs []
      = day10' xs ['E']

scoreRem :: [Char] -> Int
scoreRem (p : ps)
  | p == ')' = 1 + 5 * scoreRem ps
  | p == ']' = 2 + 5 * scoreRem ps
  | p == '}' = 3 + 5 * scoreRem ps
  | p == '>' = 4 + 5 * scoreRem ps
  | p == 'E' = 0 + 1 * scoreRem ps
  | otherwise = error "Unexpected character"
scoreRem _
  = 0



day10In :: String
day10In = [r|[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]|]

day10InFull :: String
day10InFull = [r|[({<(({{(([([[{}{}](<>())][<(){}>[[][]]]){{(<>{})<{}()>}}](<<<()<>><()<>>>([<>[]])><{<[][]
<<([{([<([{<<{[]<>}([]{})>><{(<>{})([][])}[[{}]((){})]>}([[([]<>)[<><>)]{<{}[]>{[]()}}]<<[{}{}]<<>{}>>{[{}<>]
{<<[(<{<({[{[{[][]}{[]{}}](<[]<>>[<><>])}]}[<{(<<>{}>(()()))<(<>{})(<>())>}>{{{{()()}{<>}}}}])><<{{<{[
{({([<<<[<<<<[{}{}][<>]>[(<><>)]>[<{{}()}<[]<>>>]>>([{[[()<>]{[]<>}](([]{})[<>()])}(((())[{}])[{<>()}
[{[<[{[(([{({[()<>](<>())}[{<><>}[[]()]])((<{}[]><()[]>>[[<>{}]{<><>}])}<[<{{}[]}{()()}>{(
{<<{{[{{({{((([]()){{}()})){(<{}[]><()>)(<<>[]><[]<>>)}}[[[<()<>>][<()<>>(()())]]<[[[]<>]{
[{({[{[{[[<[<[<>{}]<[]>>{<<>[]><{}[]>}]>[[[[[]<>><<>{}>](<{}[]>[{}<>])]{<{<>{}}(<><>)><<{}[]>(<><>)>}]]<{{{{[
<<<{((({<<<[[[()<>]<[]()>](<{}()>)](({[]{}}<<>()>){{[]{}}})>{[[[{}[]]<{}{}>]{[[][]]{<>()}}]}><
({[<<{<({{[[(({}[])[<>()])<<{}[]>[[]<>]>]([{{}[]}<{}[]>]<{()[]}<{}()>>)]<[{<[]()>[{}{}]}{[()[]]}]<
([([[{[((<[[<[<><>][(){}]>[(()[])]]]<{<{[][]}([]())>([{}][<>{}])}<(<[][]>[(){}])(([][])<<>()>)>>><[
{<[<<([(([<([[{}[]](()())]<{<>()}>)[<(<>{})<<>{}>>{{[][])[()<>]}]>[[({<>{}}([]{})){[()()]}][{[<>]}(
([<[<[([([[[<<<>{}>><{()()}[{}[]]>]({<<>[]>{<>()}})](((<<>{}>{{}()}))<[([]<>){{}{}}]>)][{(
[[({(<[{<(([<{(){}}>)({<()[]><<>()>}<<()[]><<>>>))[{({(){}})(<{}<>>{[]<>})}[[([]())[()[]]]]])([<
[<<<{[({[<[<{[{}[]][[]<>]}>{<{{}<>}>({()[]}[{}[]])>]([<[{}()]<{}[]>>{[{}()]}]{[[(){}]{<>}]<<<
{[<[({((([(<{({}())}([{}[]]{<>[]})><[[[]{}](()())]{{<><>}{[]{}]}>)[[({<>()}<()()>)([()<>][()<>]
<<({({[[(<(<{[<>[]]({}{})}>)[<[{(){}}[{}[]]]<{<><>}>>]>{<(([()]<(){}>)([{}<>])){(<<>()>{[]()})(<{}{}>{{}{}
<{({(<<{[[{<{[<>()]{<>[]}}>}([{{<>[]}{[]()}}{{{}{}}{<>[]}}])]<<(((<>[]))<<[]()>[()()]>)<<<{}{}>>{<()<
{([{{[{<<<{<[{()()}{<>}][{[][]}{[][]}]>}<<[((){}){{}()}]{{(){}}{[]<>}}>[{<{}[]>}<{[][]}{{}[]
{{[((<[[[<{([<()[]>[()[]]]{[<><>][<><>]})([{()}][{<>[]}{{}[]}])}[[[<{}{}>({}{})]{<()<>>({}[])}]<
{(({[((<{(<{<<<>[]>{{}<>}>}(([(){}]<[]<>>))>[[(<<>>{<><>})(({}<>)(()<>))]])}>([<[(<[{}<>][[]<>]>[(
<{{[(<<{{(<({<<><>>({}())}[<[]()><<>[]>]){{(()()){<><>}}[({}{}){[]<>}]}>{(<<{}()><()()>>{{<>{}}{{}<>
{{<{({{{([{<({{}<>}{[][]}){{()[]}}><{{[]}{<>{}}}[{()<>}[<>[]]]>}[{([()]){({}<>)[<>{}])}(<<[][]>[()[]]>([
[{<(<({<<[[(<[[]{}]({}{})><<()<>>{[]()}>)({<[]()>([]())}{{[]{}}{<>[]}})]]{[(<{{}[]}{[][]}><[(){}]<<><>>>)]}>>
{<<(<({<{[<[{<()<>>}]<{[<><>]{[]{}}}<{<><>}>>>([[<[]()><{}()>]{{<>[]}>](<[<>{}]<[]{}>>[[()[]]{[]{}}])
<({<{<[{([<(({{}()}(<>()))([<><>]{[][]}))><<[<[][]>{()()}]>>]){[[({{[]}}{[()()]}){[(()[])[{}()]](([]<>){<><
[({{([<[({<((({}[])(<>()))<[<><>][{}{}]>)[[([]{})<<><>>]((<>{})([]{}))]>(([[[]()}{<><>}][{()[]}]){{<{}{
<{({[[{({((({([]<>){<>[]}}{{<><>}})(({()<>>[[]{}])<(()[])[[]{}]>)))})[{<<({[<>()]})([[()[]]<{}
{({(<(<([{{{(<{}>)<{[]()}[[]()]>}[([<>{}]{[]()})[[{}](<>[])]]}{[({{}{}}[{}()])([{}()]{<>[]})]{[(()[])(()
{<{{<[[({[{<({()<>}{()<>})>{[{()[])[()[]]]}}][({<<[]<>>[()[]]><<{}<>>[{}()]>})]})<[({<({[]()
<<(<{[<[(({{<(()[])<{}{}>><(<>{})<{}[]>>}[<([]())({}{})><<()[]><()[]>>]}<[[{(){}}{<>()}]{((){}){<><>})]<
<<<((([{({<{<[(){}]{(){}}>}{<(()<>)<{}{}>>}>[([<<>()><<><>>])<(([]{})){<{}[]>[{}()]}>]}[{{<<()[]>(
({{([[<[[<<{({()()}({}[]))}[{(()[]){{}<>}}{([]<>)(()())}]><<{{{}[]}}{[[]](()()>}>{<({}[]){[]
<[<[[<[[<{{{<{<>()}[()[]]](<{}[]><<>[]>)}<<<{}()>([]<>)>{([]{}){[]<>}}>}([{{<><>}[[][]]}{[<>
{[[{[<{{[{{[[<{}[]><<>>]]}<<<{()()}([]<>)>{<<>[]>[<>[]]}>([{()[]}{[][]}]{{{}{}}(<>{}}})>}<((<<()[]>>[<()()><[
([(<{(({[([([[{}]])]({<(()())<<>{}>>}<<{{}()}>[<<>()><<><>>]>))<{{{{()()}[(){}]}>}>]})<[<<{<[<{}{}><{}{}>]{{
{{{<[{{([<[<([{}[]]({}<>))[([]())]>](<(<()()>[<>])>{[(()<>)]({<>[]}(<>{}))})>{({[{[]<>]<{}{}>]<[{}<>]<<
<{<<<([([{<[(([]{})(<>)){(<>{})<<><>>}]{({<>[]}{{}()}){{{}{}}{{}()}}}><[<(()){[][]}>{<{}{}>{<>()}}]>}][[
([({{({{([[{[{[]()}{[]{}}]<<[]<>>(()<>)>}[((<>{})[<>()])[[{}()]<<><>>]]]][<[<(()<>)(<>)><[<
[<<{[(<[<<{{{<<>{}><<>[]>}}}>((<[[()<>]<<>()}]<(<>()){<>()}>>([(()<>)([]{})][[(){}]{{}{}}]))[{[[[]{}]]<
<{<{<<[([[(<[[[]()]<{}{}>][{[]<>}(()[])]]({{<><>}(()[])}{([][])(()())}))(<[[<>{}]][{()<>}]>)][<{<(
[{[[[[((<[[((<[][]>{(){}})[{()()}])]{<<{(){}}>{(<><>)(()[])}){<[()]<[]()>>}}]>))[[<{([{{()<>}{()
([{({<<{[(([(<[]<>>{[]()})](<{{}{}}[()()]>)))[[<{({}())}{<()[]>}>](<([[]{}][<>{}])<{{}{}}[()[]]>>[<
([[<{[[{[[<{{[{}[]](<>{})}([()<>](()[]))}>(([{[][]}<<>()>]((<>{}){<>[]})))]([([{{}[]}([]()))(<{}()><<><>>))[
(<<[{<<<{(<<([()][(){}])({[]{}})>{<{{}<>}{()()}><[{}[]](<>[])>}>{[[{()()}[[]<>]]<<{}()><[]{}>>](<{<>[]}>{<<><
{<(<({{[({[{{{(){}}[<>[]]){[<><>]{[]()}}}<<[[]<>]{{}<>}>{((){})([][])}>]}<{<((()){[]()})<{<>(
[[[((({{[[{(<[[][]]((){})>){[([]{})]<<{}<>><()[]>>}}<((<()()>))[{([]())([][])}[<()>{(){}}]]>][{(<<<>()><(){
{<<<{{(({{{<({{}[]}({}<>))<{{}}>><({<>{}}{<>{}})>}[([[<><>]<[]>](({}{})<(){}>))([<{}[]>[()[]]]
[((([(([{{{([{<><>}]<((){}){{}[]}>)<([()[]]{[][]})<{{}()}{[][]}>>}}}(<<<<<()<>>[{}[]]>>>{<[
[{(<[[{{({(({((){})((){})}{(<>{})<[]{}>})([(()[])<()[]>]))[([{()[]}[[]<>)])<{{()<>}(()())}<{[][]}[{}<>
<(<(<([(((<{(<<>{}>[<>{}]){<{}<>>({}<>)}}(<<<>[]>[()()]><{[][]}>)>)))[<{(<{([]{})[[]{}]}[<{}{}><()()>]
<<(<([<[(<<{[[()[]]([]{})]}[[((){})<{}()>]<<[]<>>({}<>)>]><(<{<>()}<[]{}>>(<{}{}>(()<>)))>>){{([<([]{})<{}{}>
<{<{{{(({{<[[([]<>){{}<>}]{{[][]}}]>{<<{()<>}({}[])>{{{}()}{(){}}}>}}[<(<([]())<<>()>>[{[]}{<>[]}]){[[{}<
[{(<(({(<<{[[({}()){<>{})]]}[[{[<>[]][{}[]]}]((([][])<(){}>)[{[]{}}(<>())])]>>[(((({(){}}<{}>)(<{}{}><[]
{(([[[(<<<<<{<<><>><{}[]>}[[[]{}]([]<>)]>{{{<><>}<[]{}>}[<{}<>><()<>>]}>>[{{<{<>}(()<>)>[{{}<>
{([{[([<[<<[[(()()){()()}](([]()){<>()})]>({<<{}()><<>()>><[<>{}]{{}<>}>})>]{{(<(<[]<>>{<>
{(({{<[[{([[{[[]()][{}{}]}{[()<>]<()[]>}](<({}[])>{[[]{}]<<>[]>})]{{({{}<>}<(){}>){<{}()>({}{})}}[[{
{(((<<{<([[{([()[]][[]<>])}{{[<>{}]<{}()>}<<<>[]>[<>{}]]}](({[()()](<>{})}[<()>]))]{{[[[[]<>]{<>
([[[({{(([{{([<>{}]){<{}{}><{}[]>}}<[[(){}][[]{}]][(())[<>{}]]>}])[{([{{<>()}[[]<>]}{[[]()]{[][]}}]{{{
{[<(<<{<{<[([(()())[<>()]]{{{}<>}{[]<>}})]>}>}{{<([<<<[]()><{}()>>([[]()]([]()))><{<<><>>([]<>)}<(<>
<[<(((<[(<[[[<<>[]>{()[]}][{[][]>(<>)]](<{<><>}[()]>)]>{<{({{}{}}[{}{}]){[[]<>]<{}>}}[{({})
[{<[[[<[[[<[{[<>[]]{[]()}})<<[[]()]{[][]}>([[][]]{()()})>>]]([[({[<><>][<>]}{<<>()>([])})<([{}<>][<>()])<(
{((<[<(<(({([(()<>){[]}]<[{}<>]{()<>}>)([([]{})][{(){}}[[]<>]]]}<<{{()<>}(()[])}<(<><>)<()>>>>)({{[<(){}>[
[([{<((([[<<[{{}{}}({}<>)]>><<[[[]()](<>())]>>]([([({}<>)](<{}<>>))<[[{}{}]]>]<[<[[][]]{()()}><{{}<>}((){})
([[[{{[[({{{{{()<>}{[]{}}}}[<<()[]><[]{}>>[[<>()]]]}{<[{(){}}(<>())][<{}<>><[][]>]>{<([]{})[()()
[<(<<({(<[[{[{()()}{{}{}}][[<>()>]}{[[<>{}](<>[])]}]([{([][])<()>}{<<>()>[()<>]}]{{[[]][[]<>
[<<({([<{[{(([{}()]<<><>>))}[({<[]()>([]{})}[{()()}(()<>)])(<([][])(())>{<[]()>{[]{}}})]]{[<({<><>}<[]
<{({(<{{(<{([[<>{}]])<<[()[]]{<>{}}>>}{[((()<>))<(()())>][{{[]{}}[<>()]}(<<>{}>[()()])]}]{{({<{}
(<{{[<[[{[<{{[{}()]}{({}{})}}(<{[]()}({})><<[]{}>(<>[])>)>](<{[(())({}{})]((<>[]){()<>})}[{[[]<>][{}[]]}[
(<<{<[[{(([{[(<>{})([])]<[<>()][<>[]]>}]<[([[]()](()[]))({(){}}<(){}>)]<{{{}{}}<[]<>>}<([])([]<>)>>]))}]{
[([{({{((<[[([{}[]]{<>}){(()[])([]{})}][{<{}<>><()()>}]]<([(<>{})[<>[]]])]>))}[((({(([[][]
<[{<<[{{<{{[{[()()]{()[]}}][([{}<>]<()<>>)<[{}{}]>]}{<[(()<>)][((){})({}())]>((<<>{}><<>()>)<[{
([[{[<[{[<[([({}[]){{}()}]){({[][]}(()))[<[][]>{{}<>}]}]>]}(({{({<{}<>>[[]<>]}{{()}})({[<>()]}<{{}}>)}[
([[[<[<(<<<[{{[]}<<>{}>}[(<><>){{}[]}]><[{{}<>}<()()>][[{}()](<>{})]>><[{{()[]}<{}>}<[<><>]({
([(([[[<{<<<{([]){[][]}}{(<>[])[{}()]}>[<(()[])[()[]]>(<()<>>(()<>))]>([<{<>{}}<{}[]>>(<()[]><{}
(<[{<<(<<({<[(())([]<>)][<()[]><{}<>>]>({<{}{}>{{}}}{(<>[])<[]{}>})}){[({[()()][[]{}]}[<<>[]>{{}
({[{(<(([(<[{<[]<>>(()())}[(())[{}<>]]]<{{()[]}{()()}}>>[<<[{}{}][{}{}]>((()()>[{}()])>[[<<>{}>(<
(([(<<([<[({(<{}()>)({<>{}}({}<>))}[({{}<>]{[]()})<{[]<>}>])<<[{{}()}{()<>}]{<()()>{<>()}}
[((<{<([{{<<<(()[])(<>())>{<{}()>(<>())}>[<<[]>({}())>]>({<[{}<>]{<><>}>})}}])<<[<{([{[][]>{()()}
<[({(<[{{[[(((()<>)<{}[]>){<[]()>[[]()]})<[[{}()](()<>)]<((){})>>]]<(({(<>[])([]{})}[{()}[()()]
(<[{[<(<[([[(<(){}>{{}())){(<>{})[{}()]}]<<({}[]){[]()}>(({}{}){{}{}})>](<{<[][]>([]())}[<()>{<>
{<[[<((((({({[{}[]]{<><>}}({<>()}{{}<>}))([[()()]([]{})]{[{}]{<>()}})}{<((<><>)(<>[]))[[{}()][{}[]]]><<[<>{}]
[[[{{{(<<([(<(()[])<[]{}>>[[<>{}]<{}>]){<{[]()}(()<>)>([{}[]][{}<>])}])(<{<<{}[]>{<>{}}>([()[]]<{}{}>)
{<[(({[[((([{(<>[])<<>[]>}[<()[]><{}{}>]](<([]{})(<><>)>{{()}({}[])}))))([[(([<>{}][{}()]))<{{{}()}<{}[]>}
<<<<<<<[{<[({{{}[]}<<>>}[<{}<>>([])>)<{({}[])}<({}())>>]{{{{{}()}}}}>{<[(({}[])(<><>))](<{()<>}>([{}{}][[]{}
[[(<(<{<([(<[[[]{}]{[]{}}]<[{}<>]<{}<>>>><{<()[]>({}())}([()[]]{()})>){{{{<>{}}<()<>>}(<{}()>([]
(<<([<(([([{[[<>[]](<><>)][[<><>]{{}[]}]}[{<()()>}]]){([((()()){<>{}})(({}()}([]<>))]{<{<>()}(
({({<(([(([(({{}<>}{(){}}){{()[]}[{}<>]})[[{()()}{()()}]((()))>]{({([]{}){{}[]}}{<{}()>[{}()]})(<[[]{}]{
[{[{<[[((<[(((<>[])([]<>>)({()<>}))][{<({}[]){<>{}}>}(<{<><>}{{}{}}>{<()[]>{(){}}})]>))[{{{
[<(({[<<<{(<[<(){}>{[][]}][[()<>]<{}{}>]>{{<[]{}>[()[]]}{({}[]){[]<>}}})((({<><>}(()))[([]{})]))}{<<({{}{
({([<{{((<({([{}<>]([]{}))(<[]<>>{[]()})}<[([]{})[[]{}]][(()<>)<[]<>>]>)[(<{[]<>}<{}()}>[({}<>)<<>{
[<[(<{([(([(<[{}{}]{{}[]}>[([]())[()()]])(<{(){}}{{}{}}>)])([{<<[]{}>>{{[]()}[{}{}])}][{{{{}<>}}}
{(<{((<[[[({[((){})]}{<({}<>)(<>())>[<{}<>>([]<>)]})][{<{<[]{}>{()[]]}(([]{})<<>()>)>([{{}<>}<[][]>]{
<{(([<([({[(<(<>{})[[]<>]><[[][]]<{}[]>>)](<{{{}<>}(<>())}<[<><>]([]())>>([[()[]]<<>{}>]))}<
{{([<<({[[<<<{()[]}<{}()>>[[(){}>{<><>}]>[[{[]}]{({}<>){{}{}}}]>[[{<[]<>><()<>>}<{<>()}>]<{
([{<{<((([<[(<[]{}><<>()>)(({}<>)[[]<>])]><{[(()())[()()]]}>]<({{<<>[]>[<>{}]}[[()<>]]])[[<({}[])({}<>)><
[<[([(<([[<[[[()[]][[]<>]][<{}<>>]](([{}{}])[{[]{}}({})])>[<<({}()){<>[])>{<<><>>(<><>)}>]]{{{{({}<>)<<>
<<({[<{[({{[[<[]()><<>>][<()[]>[{}<>]]]}<{<[(){}]{[]{}}>({{}<>}[<>[]])}{<{[]()}<<>()>>{(<>[])[{}
{{<{({{([{<<<{<>[]}(<><>)>[<<>[]>({}{})]><<{()}[{}[]]>[<()[]>{<>{}}]>>{{[<{}()>(<>[])]}[(<[]()>(()))
[({(([(([{<<{[<>{}]({}{})}{[[]()]{[]{}}}>>}}<{<{<((){})(<>[])>{([]())<{}<>>}}<[<()>][<(){}>{
(<<[<<({([([(<[]<>>{{}()})([()<>][()<>])](<({}{}){<>{}}>{(<>{}]{{}()}}))]{{[[([]())<[]<>>]
<[[{{({{{<<[<<[][]>{<>()}>{{()()}([]<>)}]<[{()[]}{()}][[[]]]}>>}<<[{{{<>[]}<<>{}>}({{}()}{[][]}
[{<{[[<[<([((<{}<>><<><>>){<[]{}>[{}{}]})[[([])[[]()]]({<><>}<(){}>)]]{<((()[])([][])){(<>())([][])}>[{
<(([{<[<<[{<[[[][]]](<[]<>><<>[]>)>([{(){}}{{}[]}]{<<>[]><{}()>})}<{{([][])[{}<>]}{{{}()}}}[<(<
<<{((([<[(<[{<[]()>}{(())<()<>>}]>[{<[<>()]{()<>}>[{<><>}{[][]}]}{[<{}{}>({}<>)]{{[]()>{[]<>
[[[{[[[{{<(<{{[]<>}[<><>]}({()()}[()[]])>[((<>[])(<>)){{()<>}([][])}])[{{{[]<>}<()[])}({<>[]})}(({
{<<<(<(((({[[([])([][])]<<[]<>>>]}{{<{{}[]}(<>[])><{[][]}[[]{}]>}<{<()[]>}<{[]{}}{()<>}>>})){<<<[{[]<>}[[]|]

day10In' :: [String] 
day10In'
  = lines day10InFull
