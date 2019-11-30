import React from 'react'
import { H5, Button, Intent } from '@blueprintjs/core'

const RegistryNone = ({ showRegistrar }) => (
  <div>
    <H5>No OAuth applications in your account...</H5>
    <p>OAuth applications are used to access REST API.</p>
    <Button intent={Intent.PRIMARY} onClick={() => showRegistrar(true)}>
      Register New OAuth App
    </Button>
  </div>
)

export default RegistryNone
