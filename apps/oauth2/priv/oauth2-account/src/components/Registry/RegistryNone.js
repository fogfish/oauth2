import React from 'react'
import { H4, Button } from 'react-dress-code'

const RegistryNone = (props) => (
   <div>
      <H4>No OAuth applications...</H4>
      <p className="dc-p">OAuth applications are used to access REST API.</p>
      <Button primary onClick={ () => props.history.push('/oauth2/account/app') }>
        Register a new Application
      </Button>
    </div>
)

export default RegistryNone
