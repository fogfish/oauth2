import React from 'react'
import { AccessKey } from './AccessKey'
import { SecretKey } from './SecretKey'

export const KeyPair = ({ response_type, client_id, state }) => (
  <div>
    <AccessKey />
    <SecretKey />
    <input name="response_type" type="hidden" value={response_type}/>
    <input name="client_id" type="hidden" value={client_id} />
    <input name="state" type="hidden" value={state} />
  </div>
)
