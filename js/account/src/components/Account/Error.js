import React from 'react'
import { Callout, Intent } from '@blueprintjs/core'

const Error = ({ error }) => ( 
  <Callout intent={Intent.DANGER} title="Login failed.">
    Your session is expired, please sign-in again!
  </Callout>
) 

export default Error
