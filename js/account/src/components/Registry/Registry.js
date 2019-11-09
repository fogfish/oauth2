import React, { useState, useEffect } from 'react'
import { Card, H2, Button, Intent, Spinner } from '@blueprintjs/core'
import { secureLookup } from '../OAuth2'
import { Issue } from '../Issue'
import RegistryNone from './RegistryNone'
import RegistryList from './RegistryList'
import { WhileIO, SUCCESS, PENDING, FAILURE } from '../WhileIO'

//
const Head = ({ history, status }) => (
  <H2 style={{display: 'flex', justifyContent: 'space-between'}}>
    OAuth Apps
    {status === SUCCESS &&
      <Button 
        minimal
        small
        intent={Intent.PRIMARY}
        onClick={ () =>history.push('/oauth2/account/app') }
      >
        New OAuth App
      </Button>
    }
  </H2>
)

//
const Registry = ({ apps }) => 
  apps.length > 0 ? <RegistryList apps={apps} /> : <RegistryNone apps={apps} />

const IO = WhileIO(Spinner, Issue, Registry)

const RegistryWithData = () => {
  const [registry, updateRegistry] = useState({status: PENDING, apps: undefined})
  useEffect(() => { lookup(updateRegistry) }, [])
  
  return (
    <Card>
      <Head { ...registry }/>
      <IO { ...registry }/>
    </Card>)
}

const lookup = async (updateRegistry) => {
  updateRegistry({status: PENDING, apps: undefined})
  try {
    const apps = await secureLookup('https://pr15.auth.fog.fish/oauth2/client')
    console.log(apps)
    updateRegistry({status: SUCCESS, apps})
  } catch (error) {
    updateRegistry({status: FAILURE, error})
  }
}

export default RegistryWithData