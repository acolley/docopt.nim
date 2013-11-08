import os
import re
import sequtils
import strutils
import tables
import typetraits

{.warning[SmallLshouldNotBeUsed]: off.}

type
  EDocoptLanguageError = object of E_Base

  TPatternKind = enum
    BranchPattern,
    LeafPattern

  TBranchPatternKind = enum
    Required,
    Optional,
    OptionsShortcut,
    OneOrMore,
    Either

  TLeafPatternKind = enum
    Argument,
    Command,
    Option

  TPattern = ref object
    case kind: TPatternKind
    of BranchPattern:
      children: seq[TPattern]
      branchKind: TBranchPatternKind
    of LeafPattern:
      name, value: string
      case leafKind: TLeafPatternKind
      of Option:
        short, long: string
        argcount: int
      else: nil

  TTokens = seq[string]
  
# TODO: add these to std lib?
proc isUpper(s: string): bool =
  result = s == s.toUpper()

proc lstrip(s: string, c=' '): string =
  var i = 0
  while s[i] == c:
    inc(i)
  result = substr(s, i, len(s)-1)

proc lstrip(s: string, cs: set[char]): string =
  var i = 0
  while s[i] in cs:
    inc(i)
  result = substr(s, i, len(s)-1)

proc rstrip(s: string, c=' '): string =
  var i = len(s)-1
  while s[i] == c:
    dec(i)
  result = substr(s, 0, i)

proc rstrip(s: string, cs: set[char]): string =
  var i = len(s)-1
  while s[i] in cs:
    dec(i)
  result = substr(s, 0, i)

# TPattern implementation

#proc match(patt: TPattern, left: seq[TPattern], coll: seq[TPattern]):
#  tuple[success: bool, l, c: seq[TPattern]]

proc matchRequired(patt: TPattern, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
  assert(patt.kind == BranchPattern and patt.branchKind == Required)
  var collected = coll
  if collected == nil:
    collected = @[]

  var 
    l = left
    c = collected
  for pattern in patt.children:
    let matched = pattern.match(l, c)
    var success = matched[0]
    l = matched[1]
    c = matched[2]
    if success:
      return (false, left, collected)
  return (true, l, c)

proc matchOptional(patt: TPattern, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
  assert(patt.kind == BranchPattern and patt.branchKind in @[Optional, OptionsShortcut])
  var collected = coll
  if collected == nil:
    collected = @[]

  var
    l = left
    c = collected
  for pattern in patt.children:
    var matched = pattern.match(l, c)
    l = matched[1]
    c = matched[2]
  result = (true, l, c)

proc matchOneOrMore(patt: TPattern, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
  assert(patt.kind == BranchPattern and patt.branchKind == OneOrMore)
  assert(len(patt.children) == 1)
  var collected = coll
  if collected == nil:
    collected = @[]
  var
    l = left
    c = collected
    ltemp: seq[TPattern] = nil
    matched = true
    times = 0
  while matched:
    # could it be that something didn't match but changed l or c?
    var res = patt.children[0].match(l, c)
    matched = res[0]
    l = res[1]
    c = res[2]
    if matched:
      inc(times)
    if ltemp == l:
      break
    ltemp = l
  if times >= 1:
    result = (true, l, c)
  else:
    result = (false, left, collected)

proc matchEither(patt: TPattern, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
  assert(patt.kind == BranchPattern and patt.branchKind == Either)
  var collected = coll
  if collected == nil:
    collected = @[]

  var outcomes: seq[tuple[success: bool, l, c: seq[TPattern]]] = @[]
  for pattern in patt.children:
    var outcome = pattern.match(left, collected)
    if outcome[0]:
      outcomes = outcomes & outcome

  if len(outcomes) > 0:
    # return smallest matching outcome pattern
    # i.e. the one with the fewest left patterns
    result = outcomes[0]
    var i = 1
    while i < len(outcomes):
      if len(outcomes[i][1]) < len(result[1]):
        result = outcomes[i]
  else:
    result = (false, left, collected)

proc singleMatchArgument(patt: TPattern, left: seq[TPattern]):
  tuple[pos: int, patt: TPattern] =
  assert(patt.kind == LeafPattern and patt.leafKind == Argument)
  var i = 0
  for pattern in left:
    if pattern.kind == LeafPattern and
       pattern.leafKind == Argument:
      return (i, TPattern(kind: LeafPattern, leafKind: Argument, name: patt.name, value: pattern.value))
  return (-1, nil)

proc singleMatchCommand(patt: TPattern, left: seq[TPattern]):
  tuple[pos: int, patt: TPattern] =
  assert(patt.kind == LeafPattern and patt.leafKind == Command)
  var i = 0
  for pattern in left:
    if pattern.kind == LeafPattern and
       pattern.leafKind == Argument:
      if pattern.value == patt.name:
        return (i, TPattern(kind: LeafPattern, leafKind: Command, name: patt.name, value: "true"))
      else:
        break
  return (-1, nil)

proc singleMatchOption(patt: TPattern, left: seq[TPattern]):
  tuple[pos: int, patt: TPattern] =
  assert(patt.kind == LeafPattern and patt.leafKind == Option)
  var i = 0
  for pattern in left:
    if patt.name == pattern.name:
      return (i, pattern)
    inc(i)
  return (-1, nil)

proc matchLeafPattern(patt: TPattern, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
  # TODO: finish
  assert(patt.kind == LeafPattern)
  var collected = coll
  if collected == nil:
    collected = @[]

  var matched: tuple[pos: int, patt: TPattern]
  case patt.leafKind
  of Argument: matched = patt.singleMatchArgument(left)
  of Command: matched = patt.singleMatchCommand(left)
  of Option: matched = patt.singleMatchOption(left)
  
  var (pos, match) = matched
  if match == nil:
    return (false, left, collected)
  
  var lefttmp = left[0..pos] & left[pos+1..len(left)-1]
  var sameName = filter(collected) do (pt: TPattern) -> bool: pt.name == patt.name
  result = (false, @[], @[])

proc `$`(patt: TPattern): string =
  case patt.kind
  of BranchPattern:
    result = $patt.branchKind & ": (" & join(map(patt.children, proc(p: TPattern): string = $p), ", ") & ")"
  of LeafPattern:
    case patt.leafKind
    of Option:
      result = "Option(" & patt.short & ", " & patt.long & ", " & $patt.argcount & ", " & patt.value & ")"
    else:
      result = $patt.leafKind & "(" & patt.name & ", " & patt.value & ")"

proc flat(patt: TPattern, types: openarray[string]): seq[TPattern] =
  case patt.kind
  of BranchPattern:
    if $patt.branchKind in types:
      result = @[patt]
    else:
      result = @[]
      for child in patt.children:
        result = result & child.flat(types)
  of LeafPattern:
    if len(types) == 0 or $patt.leafKind in types:
      result = @[patt]
    else:
      result = @[]

proc match(patt: TPattern, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
  case patt.kind
  of BranchPattern:
    case patt.branchKind
    of Required: result = patt.matchRequired(left, coll)
    of Optional, OptionsShortcut: result = patt.matchOptional(left, coll)
    of OneOrMore: result = patt.matchOneOrMore(left, coll)
    of Either: result = patt.matchEither(left, coll)
  of LeafPattern:
    result = patt.matchLeafPattern(left, coll)

proc singleMatch(patt: TPattern, left: seq[TPattern]):
  tuple[pos: int, patt: TPattern] =
  assert(patt.kind == LeafPattern)
  case patt.leafKind
  of Argument: result = patt.singleMatchArgument(left)
  of Command: result = patt.singleMatchCommand(left)
  of Option: result = patt.singleMatchOption(left)

# END TPattern implementation

# END TLeafPattern implementation

# TArgument implementation

proc parseArgument(source: string): TPattern =
  var name = findAll(source, re(r"<\S*?>", {}))[0]
  var m = findAll(source, re(r"\[default: (.*)\]", {reIgnoreCase}))
  if m[0] =~ re(r"\[default: (.*)\]", {}):
    result = TPattern(kind: LeafPattern, leafKind: Argument, name: name, value: matches[0])
  else:
    result = TPattern(kind: LeafPattern, leafKind: Argument, name: name, value: "")

# END TArgument implementation

# TCommand implementation

# TODO: a command is a flag so the value
# should be a bool, find a way to represent
# this in this statically typed language

# END TCommand

# TOption implementation
proc parseOption(optdesc: string): TPattern =
  var
    short = ""
    long = ""
    argcount = 0
    value = ""

  var (options, ign, desc) = optdesc.strip().partition("  ")
  options = options.replace(",", " ").replace("=", " ")
  for s in options.split(' '):
    if s.startswith("--"):
      long = s
    elif s.startswith("-"):
      short = s
    else:
      argcount = 1

  if argcount > 0:
    var matched = findAll(desc, re(r"\[default: (.*)\]", {reIgnoreCase}))
    if len(matched) > 0:
      # bad hack! because findAll does not return sub matches
      # we have to match against the first returned match
      if matched[0] =~ re(r"\[default: (.*)\]", {reIgnoreCase}):
        value = matches[0]
  
  result = TPattern(kind: LeafPattern, leafKind: Option, short: short, 
                    long: long, argcount: argcount, value: value)

# END TOption implementation

# TTokens implementation
proc current(tokens: TTokens): string =
  if len(tokens) > 0:
    result = tokens[0]
  else:
    result = ""

proc move(tokens: var TTokens): string =
  if len(tokens) > 0:
    result = tokens[0]
    system.delete(tokens, 0)
  else:
    result = ""
  echo(result)

# END TTokens implementation

proc removeDupes(xs: seq[TPattern]): seq[TPattern] =
  result = @[]
  for x in xs:
    let dupes = filter(result) do (p: TPattern) -> bool: $p == $x
    if len(dupes) == 0:
      result = result & x

iterator walk[T](s: seq[T], stride=1, start=0): T =
  ## walk through a sequence with given stride
  assert(stride > 0) # cannot be neg or 0 otherwise inf loop
  var i = start
  while i < len(s):
    yield s[i]
    i += stride

proc walk[T](s: seq[T], stride=1, start=0): seq[T] =
  accumulateResult(walk(s, stride, start))

proc parseLong(tokens: var TTokens, options: var seq[TPattern]): seq[TPattern] =
  ## long ::= "--" chars [ ( " " | "=" ) chars ] ;
  var 
    part = move(tokens).partition("=")
    long = part[0]
    eq = part[1]
    value = part[2]
  assert(long.startswith("--"))
  var similar = filter(options) do (opt: TPattern) -> bool:
    opt.long == long
  var opt: TPattern = nil
  # TODO: support DocoptExit error here
  if len(similar) > 1: # might be simply specified ambiguously 2+ times?
    var longs = map(similar) do (opt: TPattern) -> string: opt.long
    raise newException(EDocoptLanguageError, long & " is not a unique prefix: " &
                                             join(longs, ", ") & "?")
  elif len(similar) < 1:
    var argcount = 0
    if eq == "=":
      argcount = 1
    opt = TPattern(kind: LeafPattern, leafKind: Option, short: "", long: long, argcount: argcount, value: "")
    options = options & opt
    # TODO: support DocoptExit error here
  else:
    opt = TPattern(kind: LeafPattern, leafKind: Option, short: similar[0].short, long: similar[0].long,
                  argcount: similar[0].argcount, value: similar[0].value)
    if opt.argcount == 0:
      if value != "":
        raise newException(EDocoptLanguageError, opt.long & " must not have an argument")
    else:
      if value == "":
        if current(tokens) in @["", "--"]:
          raise newException(EDocoptLanguageError, opt.long & " requires argument")
        value = move(tokens)
    # TODO: support DocoptExit error here

  if opt != nil:
    result = @[opt]
  else:
    result = @[]

proc parseShorts(tokens: var TTokens, options: var seq[TPattern]): seq[TPattern] =
  var token = move(tokens)
  assert(token.startswith("-") and not token.startswith("--"))
  var left = token.lstrip('-')
  result = @[]
  var opt: TPattern = nil
  while left != "":
    var
      short = "-" & $left[0]
      similar = filter(options) do (opt: TPattern) -> bool: opt.short == short
    left = left[1..len(left)-1]
    if len(similar) > 1:
      raise newException(EDocoptLanguageError, short & " is specified ambiguously " &
                                               $len(similar) & " times")
    elif len(similar) < 1:
      opt = TPattern(kind: LeafPattern, leafKind: Option, short: short, long: "", argcount: 0, value: "")
      options = options & opt
      # TODO: if the error is DocoptExit
      # we need to support this
    else: # why is copying necessary here?
      opt = TPattern(kind: LeafPattern, leafKind: Option, short: short, long: similar[0].long,
                        argcount: similar[0].argcount, value: similar[0].value)
      var value = ""
      if opt.argcount != 0:
        if left == "":
          if current(tokens) in @["", "--"]:
            raise newException(EDocoptLanguageError, short & " requires argument")
          value = move(tokens)
        else:
          value = left
          left = ""
      # TODO: support the error being DocoptExit here

    if opt != nil:
      result = result & opt
      opt = nil

# forward declaration for parseAtom
proc parseExpr(tokens: var TTokens, options: var seq[TPattern]): seq[TPattern]
proc parseAtom(tokens: var TTokens, options: var seq[TPattern]): seq[TPattern] =
  ## atom ::= "(" expr ")" | "[" expr "]" | "options"
  ##       | long | shorts | argument | command ;
  var token = current(tokens)
  if token in @["(", "["]:
    discard move(tokens)
    var matching = ""
    if token == "(":
      result = @[TPattern(kind: BranchPattern, branchKind: Required, children: parseExpr(tokens, options))]
      matching = ")"
    else:
      result = @[TPattern(kind: BranchPattern, branchKind: Optional, children: parseExpr(tokens, options))]
      matching = "]"
    if move(tokens) != matching:
      raise newException(EDocoptLanguageError, "unmatched '" & token & "'")
  elif token == "options":
    discard move(tokens)
    result = @[TPattern(kind: BranchPattern, branchKind: OptionsShortcut)]
  elif token.startswith("--") and token != "--":
    result = parseLong(tokens, options)
  elif token.startswith("-") and token notin @["-", "--"]:
    result = parseShorts(tokens, options)
  elif token.startswith("<") and token.endswith(">") or token.isUpper():
    result = @[TPattern(kind: LeafPattern, leafKind: Argument, name: move(tokens), value: "")]
  else:
    result = @[TPattern(kind: LeafPattern, leafKind: Command, name: move(tokens), value: "")]

proc parseSeq(tokens: var TTokens, options: var seq[TPattern]): seq[TPattern] =
  ## seq ::= ( atom [ "..." ] )* ;
  result = @[]
  while current(tokens) notin @["", "]", ")", "|"]:
    var atom = parseAtom(tokens, options)
    if current(tokens) == "...":
      atom = @[TPattern(kind: BranchPattern, branchKind: OneOrMore, children: atom)]
      discard move(tokens)
    result = result & atom

proc parseExpr(tokens: var TTokens, options: var seq[TPattern]): seq[TPattern] =
  ## expr ::= seq ( "|" seq )* ;
  var sequence = parseSeq(tokens, options)
  if current(tokens) != "|":
    return sequence
  
  if len(sequence) > 1:
    result = @[TPattern(kind: BranchPattern, branchKind: Required, children: sequence)]
  else:
    result = sequence

  while current(tokens) == "|":
    discard move(tokens)
    sequence = parseSeq(tokens, options)
    if len(sequence) > 1:
      result = result & TPattern(kind: BranchPattern, branchKind: Required, children: sequence)
    else:
      result = result & sequence
  if len(result) > 1:
    result = @[TPattern(kind: BranchPattern, branchKind: Either, children: sequence)]

proc parsePattern(source: string, options: var seq[TPattern]): TPattern =
  # parse from pattern into tokens
  var src = source.replacef(re(r"([\[\]\(\)\|]|\.\.\.)", {}), " $1 ") # space delimit tokens
  # FIXME: may not be correct as original regex here is r"\s+|(\S*<.*?>)" but because
  # Nimrod's split regex does not return captures in the split regex we have to use
  # this more naive method (could cause problems in future)
  var tokens = src.split(re(r"\s+", {reDotAll}))

  let res = parseExpr(tokens, options)
  if current(tokens) != "":
    raise newException(EDocoptLanguageError, "unexpected ending: " & join(tokens, " "))
  result = TPattern(kind: BranchPattern, branchKind: Required, children: res)

proc parseArgv(tokens: var seq[string], options: var seq[TPattern], optionsFirst=false): seq[TPattern] =
  ## Parse command-line argument vector.
  ##
  ## If optionsFirst:
  ##   argv ::= [ long | shorts ]* [ argument ]* [ "--" [ argument ]* ] ;
  ## else:
  ##   argv ::= [ long | shorts | argument ]* [ "--" [ argument ] * ] ;
  result = @[]
  while current(tokens) != "":
    if current(tokens) == "--":
      return result & map(tokens) do (v: string) -> TPattern:
        TPattern(kind: LeafPattern, leafKind: Argument, name: "", value: v)
    elif current(tokens).startswith("--"):
      result = result & parseLong(tokens, options)
    elif current(tokens).startswith("-") and current(tokens) != "-":
      result = result & parseShorts(tokens, options)
    elif optionsFirst:
      return result & map(tokens) do (v: string) -> TPattern:
        TPattern(kind: LeafPattern, leafKind: Argument, name: "", value: v)
    else:
      result = result & TPattern(kind: LeafPattern, leafKind: Argument, name: "", value: move(tokens))

proc parseSection(name: string, source: string): seq[string] =
  result = @[]
  let found = findAll(source, re(r"^([^\n]*" & name & r"[^\n]*\n?(?:[ \t].*?(?:\n|$))*)", {reIgnoreCase, reMultiLine}))
  for s in found:
    result = result & strip(s)

proc parseDefaults(doc: string): seq[TPattern] =
  result = @[]
  for s in parse_section("options:", doc):
    # FIXME: corner case "bla: options: --foo
    let post = partition(s, ':')[2] # get rid of "options:"
    
    # docopt.py regex is "\n[ \t]*(-\S+?)" the reason for the
    # positive lookahead here is that Nimrod's regexes don't
    # return elements that were in matching parens
    # TODO: maybe a useful addition to re.nim?
    var splitStr = split("\n" & post, re(r"\n[ \t]*(?=-\S+?)", {}))
    splitStr = splitStr[1..len(splitStr)-1]

    result = result & map(filter(splitStr, proc(s: string): bool = s.startswith("-")), parseOption)

proc formalUsage(sec: string): string =
  var section = split(sec, ':')[1] # drop "usage:"
  var pu = section.split(re(r"\s+", {}))

  pu = map(pu[1..len(pu)-1]) do (s: string) -> string:
    if s == pu[0]: ") | ("
    else: s

  result = "( " & join(pu, " ") & " )"

proc docopt*(doc: string, argv: seq[string]=nil, help=true, version="", optionsFirst=false): TTable[string, string] =
  # TODO: finish
  result = initTable[string, string]()

  var args = argv
  if argv == nil:
    args = @[]
    for i in countup(0, paramCount()):
      args = args & paramStr(i)

  let usageSections = parseSection("usage:", doc)
  if len(usageSections) == 0:
    raise newException(EDocoptLanguageError, "\"usage:\" (case-insensitive) not found.")
  if len(usageSections) > 1:
    raise newException(EDocoptLanguageError, "More than one \"usage:\" (case-insensitive).")
  # TODO: DocoptExit.usage = usage_sections[0]

  var options = parseDefaults(doc)
  let pattern = parsePattern(formalUsage(usageSections[0]), options)
  let argv = parseArgv(args, options, optionsFirst)
  # TODO: turn this into a set of TOptions
  let patternOptions = removeDupes(pattern.flat(@["TOption"]))
  var optionShortcuts = pattern.flat(@["TOptionsShortcut"])
  # mutate optionShortcuts' children
  #map(optionShortcuts) do (ops: TPattern) -> TPattern:
  #  TOptionsShortcut(ops).children = 
