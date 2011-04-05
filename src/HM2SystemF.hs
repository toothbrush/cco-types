import CCO.Component    (Component, component, printer, ioWrap)
import CCO.HM2SystemF.Base
import CCO.HM.Base
import CCO.SystemF.Base
import CCO.Tree         (ATerm, toTree, parser, fromTree)
import Control.Arrow    (arr, (>>>))

main = ioWrap $
        CCO.Tree.parser >>> 
        (component toTree :: Component ATerm HMTm) >>>
        convertAndType >>> 
        (arr fromTree) >>>
        printer

-- | constructs a really simple term, for testing.
convertAndType :: Component HMTm SFTm
convertAndType = component $ (\s -> do return (runAlgoW s))
