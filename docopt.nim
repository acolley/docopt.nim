import os
import tables

type
  EDocoptLanguageError = object of E_Base

proc parseSection(name: string, source: string): string =
  result = ""

proc docopt*(doc: string, argv: seq[string]=nil, help=true, version="", options_first=false): TTable[string, string] =
  result = initTable[string, string]()

  var args = argv
  if argv == nil:
    args = @[]
    for i in countup(0, paramCount()):
      args = args & paramStr(i)

  for arg in args:
    echo(arg)

  let usageSections = parseSection("usage:", doc)
  if usageSections.len() == 0:
    raise newException(EDocoptLanguageError, """"usage:" (case-insensitive) not found.")
