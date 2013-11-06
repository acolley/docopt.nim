import os
import re except match
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

#method fix(patt: ref TBranchPattern): ref TPattern =
# fix identities

# fix repeating arguments

# END TPattern implementation

# TLeafPattern implementation
method name(patt: TLeafPattern): string =
  result = patt.name

method `$`(patt: TLeafPattern): string =
  result = patt.name
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
    let matched = findAll(desc, re(r"\[default: (.*)\]", {reIgnoreCase}))
    if len(matched) > 0:
      value = matched[0]
  
  result = TOption(short: short, long: long, argcount: argcount, value: value)

#method single_match(opt: TOption, left: seq[TPattern])

method name(opt: TOption): string =
  if opt.long != "":
    result = opt.long
  else:
    result = opt.short

# END TOption implementation

iterator walk[T](s: seq[T], stride=1, start=0): T =
  ## walk through a sequence with given stride
  assert(stride > 0) # cannot be neg or 0 otherwise inf loop
  var i = start
  while i < len(s):
    yield s[i]
    i += stride

proc walk[T](s: seq[T], stride=1, start=0): seq[T] =
  accumulateResult(walk(s, stride, start))

#proc match(pattern: ref TPattern): tuple[res: bool, left: seq[TPattern], coll: seq[TPattern]] =
#  result = nil

#proc parse_pattern(source: string, options: seq[string]): TPattern =
#  result = nil

proc parseSection(name: string, source: string): seq[string] =
  result = @[]
  let found = findAll(source, re(r"^([^\n]*" & name & r"[^\n]*\n?(?:[ \t].*?(?:\n|$))*)", {reIgnoreCase, reMultiLine}))
  for s in found:
    result = result & strip(s)

proc parseDefaults(doc: string): seq[TOption] =
  result = @[]
  for s in parse_section("options:", doc):
    # FIXME corner case "bla: options: --foo
    let post = partition(s, ':')[2] # get rid of "options:"
    
    # docopt.py regex is "\n[ \t]*(-\S+?)" the reason for the
    # positive lookahead here is that Nimrod's regexes don't
    # return elements that were in matching parens
    # TODO: maybe a useful addition to re.nim?
    var splitStr = split("\n" & post, re(r"\n[ \t]*(?=-\S+?)", {}))
    splitStr = splitStr[1..len(splitStr)-1]
    #var elems: seq[string] = @[]
    #for stup in zip(walk(splitStr, stride=2), walk(splitStr, stride=2, start=1)):
    #  elems = elems & (stup[0] & stup[1])

    result = result & map(filter(splitStr, proc(s: string): bool = s.startswith("-")), parseOption)

proc formalUsage(s: string): string =
  var section = split(s, ':')[1] # drop "usage:"
  let pu = section.split(' ')
  result = section

proc docopt*(doc: string, argv: seq[string]=nil, help=true, version="", optionsFirst=false): TTable[string, string] =
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
  #let pattern = parse_pattern(formal_usage(usage_sections[0]), options)
