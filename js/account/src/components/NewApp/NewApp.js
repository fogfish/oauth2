import React, { useState, useCallback } from 'react'
import { Dialog } from '@blueprintjs/core'
import { KeyPair } from './KeyPair'
import { Registrar } from './Registrar'
import { secureCreate } from '../OAuth2'

const NewApp = ({ registrar, showRegistrar }) => {
  const [app, update] = useState({ identity: undefined, redirect_uri: undefined, security: 'public' })
  const commit = useCallback(
    async () => {
      const z = await secureCreate('https://pr15.auth.fog.fish/oauth2/client', app)
    },
    [app]
  )

  return (
    <Dialog
      icon="code"
      title="New OAuth App"
      canOutsideClickClose={false}
      isOpen={registrar}
      onClose={() => showRegistrar(false)}
    >
      <Registrar { ...{ app, update, commit }} />
      {/* <KeyPair /> */}
    </Dialog>
  )
}

/*
(props) => (
   <div>
      {props.keys ? <KeyPair { ...props } /> : <Registrar { ...props } />}
   </div>
)
*/

/*
const model = state => (state.app)
const actions = dispatch => bindActionCreators({}, dispatch)
export default connect(model, actions)(NewApp)
*/

export default NewApp
