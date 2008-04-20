module OrganizeImports where

import Cohatoe.API

resource :: Interface
resource = plugin {
  pluginMain = performOrganizeImports
}

-- TODO this is where the implementation starts. The first list elem of args
--      is the current editor content as string, and we are supposed to return
--      the new editor content, with organized imports. (If we can't handle,
--      e.g. because of a parse error, we should be gentle and return the
--      original buffer
performOrganizeImports :: [String] -> IO [String]
performOrganizeImports args = return args


