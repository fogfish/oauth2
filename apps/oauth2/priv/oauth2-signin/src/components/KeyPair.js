import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import AccessKey from './AccessKey'
import SecretKey from './SecretKey'


const KeyPair = ({response_type, client_id, state}) => (
   <div>
      <AccessKey />
      <SecretKey />
      <input name="response_type" type="hidden" value={response_type}/>
      <input name="client_id" type="hidden" value={client_id} />
      <input name="state" type="hidden" value={state} />
   </div>
)

const model = state => (state.core.authRequest)
const actions = dispatch => bindActionCreators({}, dispatch)
export default connect(model, actions)(KeyPair)
