namespace IronKernel

/// Installs parser-backed services for the full tool runtime.
module RuntimeSourceServices =

    let configure () =
        IronKernel.Runtime.configureSourceServices
            { parseExpression = Parser.readExpr
              parseExpressions = Parser.readExprListFromSource }