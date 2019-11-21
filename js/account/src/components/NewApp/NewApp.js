import React, { useState } from 'react'
import { Dialog } from '@blueprintjs/core'
import { KeyPair } from './KeyPair'
import { Registrar } from './Registrar'
import { useSecureCreate, SUCCESS, unknown } from '../OAuth2'

const emptyApp = { identity: undefined, redirect_uri: undefined, security: 'public' }

const NewApp = ({ registrar, showRegistrar, append }) => {
  const [app, update] = useState(emptyApp)
  const { status, commit } = useSecureCreate('https://pr15.auth.fog.fish/oauth2/client')
  
  return (
    <Dialog
      icon="code"
      title="New OAuth App"
      canOutsideClickClose={false}
      isOpen={registrar}
      onClose={() => showRegistrar(false)}
    >
      {!(status instanceof SUCCESS)
        ? <Registrar 
            status={status}
            app={app}
            update={update}
            commit={() => commit(app)}
          />
        : <KeyPair { ...status.content} 
            hide={() => {
              append({ access: status.content.access, ...app })
              showRegistrar(false)
              commit(undefined)
              update(emptyApp)
            }}
          />
      }
    </Dialog>
  )
}

export default NewApp
