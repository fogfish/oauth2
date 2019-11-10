import React, { useState } from 'react'
import { Dialog } from "@blueprintjs/core";
import { KeyPair } from './KeyPair'
import { Registrar } from './Registrar' 

const NewApp = ({ registrar, showRegistrar }) => {
  const [app, updateApp] = useState({ identity: undefined, endpoint: undefined, security: 'public' })

  return (
    <Dialog
      icon="code"
      title="New OAuth App"
      canOutsideClickClose={false}
      isOpen={registrar}
      onClose={() => showRegistrar(false)}
    >
      {/* <Registrar app={app} update={updateApp} /> */}
      <KeyPair />
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
