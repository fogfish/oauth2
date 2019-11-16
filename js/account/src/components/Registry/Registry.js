import React, { useState } from 'react'
import { Card, H2, Button, Intent, Spinner } from '@blueprintjs/core'
import { WhileIO, SUCCESS, useSecureLookup } from '../OAuth2'
import { Issue } from '../Issue'
import RegistryNone from './RegistryNone'
import RegistryList from './RegistryList'
import { NewApp } from '../NewApp'

//
const Head = ({ status, showRegistrar }) => (
  <H2 style={{display: 'flex', justifyContent: 'space-between'}}>
    OAuth Apps
    {status.status === SUCCESS &&
      <Button 
        minimal
        small
        intent={Intent.PRIMARY}
        onClick={ () => showRegistrar(true) }
      >
        New OAuth App
      </Button>
    }
  </H2>
)

const Registry = (props) => 
  props.registry.length > 0 ? <RegistryList { ...props } /> : <RegistryNone { ...props } />

const IO = WhileIO(Spinner, Issue, Registry)

const RegistryWithData = () => {
  const {status, content} = useSecureLookup('https://pr15.auth.fog.fish/oauth2/client')
  const [registrar, showRegistrar] = useState(false)

  return (
    <Card>
      <Head status={status} showRegistrar={showRegistrar} />
      <IO status={status} showRegistrar={showRegistrar} registry={content} />
      <NewApp registrar={registrar} showRegistrar={showRegistrar} />
    </Card>)
}

export default RegistryWithData