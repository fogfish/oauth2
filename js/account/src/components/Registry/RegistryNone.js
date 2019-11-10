import React from 'react'
import { withRouter } from 'react-router-dom'
import { H5, Button, Intent } from '@blueprintjs/core'

const RegistryNone = (props) => (
   <div>
      <H5>No OAuth applications in your account...</H5>
      <p>OAuth applications are used to access REST API.</p>
      <Button intent={Intent.PRIMARY} onClick={ () => props.history.push('/oauth2/account/app') }>
        Register New OAuth App
      </Button>
    </div>
)

export default withRouter(RegistryNone)
