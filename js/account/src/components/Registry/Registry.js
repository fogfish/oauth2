import React, { useState, useEffect } from 'react'
import { Card, H2, Button, Intent, Spinner } from '@blueprintjs/core'
import { secureLookup } from '../OAuth2'
import { Issue } from '../Issue'


// import { bindActionCreators } from 'redux'
// import { connect } from 'react-redux'
// import { lifecycle } from 'recompose'
import { LoadingBar } from 'react-dress-code'
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

const Registry = props => (
  <>
  </>
)
    // {/* {!props.apps && <LoadingBar />}
    // {(!props.apps || props.apps.length === 0) && <RegistryNone { ...props } />}
    // {(props.apps  && props.apps.length  >  0) && <RegistryList { ...props } />}     */}


const IO = WhileIO(Spinner, Issue, Registry)

const RegistryWithData = () => {
  const [registry, updateRegistry] = useState({status: PENDING, apps: undefined})
  useEffect(() => {
    lookup(updateRegistry)
  }, [])
  console.log(registry)
  return (
    <Card>
      <Head { ...registry }/>
      <IO { ...registry }/>
    </Card>)
}

const lookup = async (updateRegistry) => {
  updateRegistry({status: PENDING, apps: undefined})
  try {
    const x = await secureLookup('https://pr15.auth.fog.fish/oauth2/client')
    console.log(x)
  } catch (error) {
    updateRegistry({status: FAILURE, error})
  }
}

export default RegistryWithData