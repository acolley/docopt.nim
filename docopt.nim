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
  
# TPattern implementation

method `$`(patt: TPattern): string =
  result = "TPattern"

method name(patt: TPattern): string =
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

iterator walk[T](s: seq[T], stride=1, start=0): T =
  ## walk through a sequence with given stride
  assert(stride > 0) # cannot be neg or 0 otherwise inf loop
  var i = start
  while i < len(s):
    yield s[i]
    i += stride

proc walk[T](s: seq[T], stride=1, start=0): seq[T] =
  accumulateResult(walk(s, stride, start))

proc parseLong(tokens: var seq[string], options: seq[TOption]): seq[TPattern] =
  result = @[]

proc parseShorts(tokens: var seq[string], options: seq[TOption]): seq[TPattern] =
  result = @[]

proc parseAtom(tokens: var seq[string], options: seq[TOption]): seq[TPattern] =
  # TODO: finish
  result = @[]

proc parseSeq(tokens: var seq[string], options: seq[TOption]): seq[TPattern] =
  # TODO: finish
  result = @[]

proc parseExpr(tokens: var seq[string], options: seq[TOption]): seq[TPattern] =
  # TODO: finish
  result = @[]

proc parsePattern(source: string, options: seq[TOption]): TPattern =
  # TODO: finish
  # parse from pattern into tokens
  var src = source.replacef(re(r"([\[\]\(\)\|]|\.\.\.)", {}), " $1")
  var tokens = src.split(re(r"\s+|(\S*<.*?>)", {reDotAll}))

  let res = parseExpr(tokens, options)
  if len(res) > 0:
    raise newException(EDocoptLanguageError, "unexpected ending: " & join(tokens, " "))

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
  #let pattern = parsePattern(formalUsage(usageSections[0]), options)
