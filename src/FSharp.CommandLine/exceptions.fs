namespace FSharp.CommandLine

exception OptionParseFailed of summary:CommandOptionSummary * msg: string with
  override this.Message = this.msg
exception CommandExecutionFailed of msg: string with
  override this.Message = this.msg

exception RequestExit of int

