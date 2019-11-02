import React from 'react'
import { H5, Button, Intent } from '@blueprintjs/core'

const RegistryNone = (props) => (
   <div>
      <H5>No OAuth applications...</H5>
      <p>OAuth applications are used to access REST API.</p>
      <Button intent={Intent.PRIMARY} onClick={ () => props.history.push('/oauth2/account/app') }>
        Register a new Application
      </Button>
    </div>
)

export default RegistryNone
