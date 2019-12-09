import React from 'react'
import AccessKey from './AccessKey'
import SecretKey from './SecretKey'

const KeyPair = ({ oauth2 }) => (
  <div>
    <AccessKey />
    <SecretKey />
    <input name="response_type" type="hidden" value={oauth2.responseType} />
    <input name="client_id" type="hidden" value={oauth2.clientId} />
    <input name="state" type="hidden" value={oauth2.state} />
  </div>
)

export default KeyPair
