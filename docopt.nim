import os
import re
import sequtils
import strutils
import tables

type
  EDocoptLanguageError = object of E_Base

  # nimrod's algebraic datatypes are 'case objects'
  # represents option types
  #TOptionKind* = enum
  #  OptString,
  #  OptSwitch

  #TOption* = object
  #  case kind: TOptionKind
  #  of OptString:
  #    valStr: string
  #  of OptSwitch:
  #    valBool: bool

  # NOTE: can also use {.inheritable.} and not have to inherit
  # from TObject to allow inheritance
  TPattern = ref object of TObject

  TBranchPattern = ref object of TPattern
    children: seq[TPattern]

  TLeafPattern = ref object of TPattern
    name, value: string

  TArgument = ref object of TLeafPattern
  TCommand = ref object of TArgument

  TOption = ref object of TLeafPattern
    short, long: string
    argcount: int

  TRequired = ref object of TBranchPattern

  TOptional = ref object of TBranchPattern
  TOptionsShortcut = ref object of TOptional

  TOneOrMore = ref object of TBranchPattern

  TEither = ref object of TBranchPattern

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

method `$`(patt: TPattern): string =
  result = "TPattern"

method name(patt: TPattern): string =
  result = ""

method value(patt: TPattern): string =
  result = ""

#method flat(patt: TPattern, types: openarray[type]): seq[TPattern] =
#  result = @[]

method match(patt: TPattern, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
  result = (false, @[], @[])

# END TPattern implementation

# TLeafPattern implementation
method name(patt: TLeafPattern): string =
  result = patt.name

method value(patt: TLeafPattern): string =
  result = patt.value

method `$`(patt: TLeafPattern): string =
  result = patt.name

# FIXME: can't take array of 'type' this crashes the compiler
# and is not supported anyway
#method flat(patt: TLeafPattern, types: openarray[type]): seq[TPattern] =
#  if len(types) == 0 or type(patt) in types:
#    result = @[patt]
#  else:
#    result = @[]

#method match(patt: TLeafPattern, left: seq[TPattern], coll: seq[TPattern]=nil) =
# TODO: finish
#  tuple[success: bool, l, c: seq[TPattern]] =
#  var collected = coll
#  if collected == nil:
#    collected = @[]
#
#  let (pos, mtch) = patt.single_match(left)

# END TLeafPattern implementation

# TArgument implementation

proc parseArgument(source: string): TArgument =
  var name = findAll(source, re(r"<\S*?>", {}))[0]
  var m = findAll(source, re(r"\[default: (.*)\]", {reIgnoreCase}))
  if m[0] =~ re(r"\[default: (.*)\]", {}):
    result = TArgument(name: name, value: matches[0])
  else:
    result = TArgument(name: name, value: "")

method singleMatch(patt: TArgument, left: seq[TPattern]): 
  tuple[pos: int, patt: TPattern] =
  var i = 0
  for pattern in left:
    if type(pattern) is TArgument:
      return (i, TPattern(TArgument(name: patt.name, value: pattern.value)))
  return (-1, nil)

# END TArgument implementation

# TCommand implementation

# TODO: a command is a flag so the value
# should be a bool, find a way to represent
# this in this statically typed language

method singleMatch(patt: TCommand, left: seq[TPattern]):
  tuple[pos: int, patt: TPattern] =
  var i = 0
  for pattern in left:
    if type(pattern) is TArgument:
      if pattern.value == patt.name:
        return (i, TPattern(TCommand(name: patt.name, value: "true")))
      else:
        break
  return (-1, nil)

# END TCommand

# TOption implementation
proc parseOption(optdesc: string): TOption =
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
  
  result = TOption(short: short, long: long, argcount: argcount, value: value)

method name(opt: TOption): string =
  if opt.long != "":
    result = opt.long
  else:
    result = opt.short

method singleMatch(patt: TOption, left: seq[TPattern]):
  tuple[pos: int, patt: TPattern] =
  var i = 0
  for pattern in left:
    # FIXME: for some reason we can't use call style patt.name()
    # as the compiler complains about needing an identifier
    # so must use name(patt) style
    if pattern.name() == name(patt):
      return (i, pattern)
    inc(i)
  return (-1, nil)

# END TOption implementation

# TBranchPattern implementation

#method flat(patt: TBranchPattern, types: openarray[type]): seq[TPattern] =
#  if type(patt) in types:
#    result = @[patt]
#  else:
#    result = @[]
#    for child in patt.children:
#      result = result & child.flat(types)

#method fixIdentities(patt: TBranchPattern, uniq: seq[TPattern]) =

#method fixRepeatingArguments(patt: TBranchPattern): TBranchPattern =

#method fix()

# END TBranchPattern implementation

# TRequired implementation

method match(patt: TRequired, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
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

# END TRequired implementation

# TOptional implementation

method match(patt: TOptional, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
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

# END TOptional implementation

# TOneOrMore implementation

method match(patt: TOneOrMore, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
  # TODO: finish
  assert(len(patt.children) == 1)

  result = (true, @[], @[])

# END TOneOrMore implementation

# TEither implementation

method match(patt: TEither, left: seq[TPattern], coll: seq[TPattern]=nil):
  tuple[success: bool, l, c: seq[TPattern]] =
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

# END TEither

# TTokens implementation
proc current(tokens: TTokens): string =
  if len(tokens) > 0:
    result = tokens[0]
  else:
    result = ""

proc move(tokens: var TTokens): string =
  if len(tokens) > 0:
    result = tokens[0]
    tokens.del(0)
  else:
    result = ""

# END TTokens implementation

iterator walk[T](s: seq[T], stride=1, start=0): T =
  ## walk through a sequence with given stride
  assert(stride > 0) # cannot be neg or 0 otherwise inf loop
  var i = start
  while i < len(s):
    yield s[i]
    i += stride

proc walk[T](s: seq[T], stride=1, start=0): seq[T] =
  accumulateResult(walk(s, stride, start))

proc parseLong(tokens: var TTokens, options: seq[TOption]): seq[TPattern] =
  result = @[]

proc parseShorts(tokens: var TTokens, options: seq[TOption]): seq[TPattern] =
  var token = move(tokens)
  assert(token.startswith("-") and not token.startswith("--"))
  var left = token.lstrip('-')
  result = @[]
  while left != "":
    var 
      short = "-" & $left[0]
      left = left[1..len(left)-1]
      similar = filter(options) do (opt: TOption) ->
        opt.short == short

# forward declaration for parseAtom
proc parseExpr(tokens: var TTokens, options: seq[TOption]): seq[TPattern]
proc parseAtom(tokens: var TTokens, options: seq[TOption]): seq[TPattern] =
  ## atom ::= "(" expr ")" | "[" expr "]" | "options"
  ##       | long | shorts | argument | command ;
  # TODO: finish
  var token = current(tokens)
  if token in @["(", "["]:
    discard move(tokens)
    var matching = ""
    if token == "(":
      result = @[TPattern(TRequired(children: parseExpr(tokens, options)))]
      matching = ")"
    else:
      result = @[TPattern(TOptional(children: parseExpr(tokens, options)))]
      matching = "]"
    if move(tokens) != matching:
      raise newException(EDocoptLanguageError, "unmatched '" & token & "'")
  elif token == "options":
    discard move(tokens)
    result = @[TPattern(TOptionsShortcut())]
  elif token.startswith("--") and token != "--":
    result = parseLong(tokens, options)
  elif token.startswith("-") and token notin @["-", "--"]:
    result = parseShorts(tokens, options)
  elif token.startswith("<") and token.endswith(">") or token.isUpper():
    result = @[TPattern(TArgument(name: move(tokens), value: ""))]
  else:
    result = @[TPattern(TCommand(name: move(tokens), value: ""))]

proc parseSeq(tokens: var TTokens, options: seq[TOption]): seq[TPattern] =
  ## seq ::= ( atom [ "..." ] )* ;
  # TODO: finish
  result = @[]
  while current(tokens) notin @["", "]", ")", "|"]:
    var atom = parseAtom(tokens, options)
    if current(tokens) == "...":
      atom = @[TPattern(TOneOrMore(children: atom))]
      discard move(tokens)
    result = result & atom

proc parseExpr(tokens: var TTokens, options: seq[TOption]): seq[TPattern] =
  ## expr ::= seq ( "|" seq )* ;
  # TODO: finish
  var sequence = parseSeq(tokens, options)
  echo(current(tokens))
  if current(tokens) != "|":
    return sequence
  
  if len(sequence) > 1:
    result = @[TPattern(TRequired(children: sequence))]
  else:
    result = sequence

  while current(tokens) == "|":
    discard move(tokens)
    sequence = parseSeq(tokens, options)
    if len(sequence) > 1:
      result = result & TRequired(children: sequence)
    else:
      result = result & sequence
  if len(result) > 1:
    result = @[TPattern(TEither(children: sequence))]

proc parsePattern(source: string, options: seq[TOption]): TPattern =
  # parse from pattern into tokens
  var src = source.replacef(re(r"([\[\]\(\)\|]|\.\.\.)", {}), " $1")
  var tokens = src.split(re(r"\s+|(\S*<.*?>)", {reDotAll}))

  let res = parseExpr(tokens, options)
  if current(tokens) != "":
    raise newException(EDocoptLanguageError, "unexpected ending: " & join(tokens, " "))
  result = TRequired(children: res)

proc parseArgv(tokens: var seq[string], options: seq[TOption], optionsFirst=false): seq[TPattern] =
  # TODO: finish
  result = @[]

proc parseSection(name: string, source: string): seq[string] =
  result = @[]
  let found = findAll(source, re(r"^([^\n]*" & name & r"[^\n]*\n?(?:[ \t].*?(?:\n|$))*)", {reIgnoreCase, reMultiLine}))
  for s in found:
    result = result & strip(s)

proc parseDefaults(doc: string): seq[TOption] =
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

  let options = parseDefaults(doc)
  let pattern = parsePattern(formalUsage(usageSections[0]), options)
