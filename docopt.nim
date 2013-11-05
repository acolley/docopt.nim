import os
import re
import strutils
import tables

from sequtils import zip

type
  EDocoptLanguageError = object of E_Base

  # nimrod's algebraic datatypes are 'case objects'
  # represents option types
  TOptionKind* = enum
    OptString,
    OptSwitch

  TOption* = object
    case kind: TOptionKind
    of OptString:
      valStr: string
    of OptSwitch:
      valBool: bool

  TPattern = object

# these are just ripped from the standard library
# with extra 'maxsplit' feature
# TODO: contribute them back to the std lib
iterator mySplit(s: string, sep: char, maxsplit=high(int)): string =
  var last = 0
  assert('\0' != sep)
  if len(s) > 0:
    var numsplits = 0
    # `<=` is correct here for edge cases
    while last <= len(s) and numsplits < maxsplit:
      var first = last
      while last < len(s) and s[last] != sep: inc(last)
      inc(numsplits)
      yield substr(s, first, last-1)
      inc(last)

    if last < len(s):
      yield substr(s, last, len(s) - 1)

proc mySplit(s: string, sep: char, maxsplit=high(int)): seq[string] =
  accumulateResult(mySplit(s, sep, maxsplit))

proc partition(s: string, sep: char): tuple[pre: string, sep: string, post: string] =
  let parts = mySplit(s, sep, maxsplit=1)
  
  if len(parts) > 1:
    result = (parts[0], $sep, parts[1])
  else:
    result = (s, "", "")

iterator walk[T](s: seq[T], stride=1, start=0): T =
  ## walk through a sequence with given stride
  assert(stride > 0) # cannot be neg or 0 otherwise inf loop
  var i = start
  while i < len(s):
    yield s[i]
    i += stride

proc walk[T](s: seq[T], stride=1, start=0): seq[T] =
  accumulateResult(walk(s, stride, start))

#proc parse(opt: 

#proc match(pattern: TPattern): tuple[res: bool, left: seq[TPattern], coll: seq[TPattern]] =
#  result = nil

#proc parse_pattern(source: string, options: seq[string]): TPattern =
#  result = nil

proc parseSection(name: string, source: string): seq[string] =
  result = @[]

proc parse_defaults(doc: string): seq[string] =
  result = @[]
  for s in parse_section("options:", doc):
    # FIXME corner case "bla: options: --foo
    let post = partition(s, ':')[2] # get rid of "options:"

    let splitStr = split("\n" & post, re"\n[ \t]*(-\S+?)")
    var elems: seq[string] = @[]
    for stup in zip(walk(splitStr), walk(splitStr, start=1)):
      elems = elems & (stup[0] & stup[1])



proc formal_usage(s: string): string =
  var section = mySplit(s, ':')[1]
  result = section

proc docopt*(doc: string, argv: seq[string]=nil, help=true, version="", options_first=false): TTable[string, string] =
  result = initTable[string, string]()

  var args = argv
  if argv == nil:
    args = @[]
    for i in countup(0, paramCount()):
      args = args & paramStr(i)

  let usageSections = parseSection("usage:", doc)
  if usageSections.len() == 0:
    raise newException(EDocoptLanguageError, "\"usage:\" (case-insensitive) not found.")
  if usageSections.len() > 1:
    raise newException(EDocoptLanguageError, "More than one \"usage:\" (case-insensitive).")
  # TODO: DocoptExit.usage = usage_sections[0]

  let options = parse_defaults(doc)
  #let pattern = parse_pattern(formal_usage(usage_sections[0]), options)
