import React, { useState, useEffect } from 'react'
import { Card, H2, Button, Intent, Spinner } from '@blueprintjs/core'
import { WhileIO, SUCCESS, useSecureLookup, useSecureRemove } from '../OAuth2'
import { Issue } from '../Issue'
import RegistryNone from './RegistryNone'
import RegistryList from './RegistryList'
import { NewApp } from '../NewApp'

const OAUTH2_CLIENT = process.env.REACT_APP_OAUTH2_CLIENT

//
const Head = ({ status, showRegistrar }) => (
  <H2 style={{display: 'flex', justifyContent: 'space-between'}}>
    OAuth Apps
    {status instanceof SUCCESS &&
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
  props.content && props.content.length > 0 
    ? <RegistryList { ...props } /> 
    : <RegistryNone { ...props } />

const IO = WhileIO(Spinner, Issue, Registry)

const RegistryWithData = () => {
  const { status } = useSecureLookup(OAUTH2_CLIENT)
  const [ registry, updateRegistry ] = useState(status)
  const [ registrar, showRegistrar ] = useState(false)

  useEffect(() => {
    updateRegistry(status)
  }, [status])

  const revoke = (id) => {
    updateRegistry(
      new SUCCESS(registry.content.filter(x => x.access !== id))
    )
  }

  const append = (app) => {
    updateRegistry(
      new SUCCESS(registry.content.concat([app]))
    )
  }

  return (
    <Card>
      <Head status={status} showRegistrar={showRegistrar} />
      <IO status={registry} showRegistrar={showRegistrar} revoke={revoke} />
      <NewApp registrar={registrar} showRegistrar={showRegistrar} append={append} />
    </Card>)
}

export default RegistryWithData