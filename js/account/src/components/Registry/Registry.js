import React, { useState, useEffect } from 'react'
import { Card, H2, Button, Intent, Spinner } from '@blueprintjs/core'


// import { bindActionCreators } from 'redux'
// import { connect } from 'react-redux'
// import { lifecycle } from 'recompose'
import { LoadingBar } from 'react-dress-code'
import RegistryNone from './RegistryNone'
import RegistryList from './RegistryList'
import { WhileIO, SUCCESS, PENDING } from '../WhileIO'
// import { lookup } from './ducks'

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


const IO = WhileIO(Spinner, Registry, Registry)

const RegistryWithData = () => {
  const [registry, updateRegistry] = useState({status: PENDING, apps: undefined})
  useEffect(async () => lookup(updateRegistry), [])
  return (
    <Card>
      <Head { ...registry }/>
      <IO { ...registry }/>
    </Card>)
}

const lookup = async (updateRegistry) => {
  updateRegistry({status: PENDING, apps: undefined})
}


// const RegistryWithData = lifecycle({
//   componentWillMount() {
//     if (!this.props.apps)
//       this.props.lookup()
//   }
// })(Registry)


// const model = state => (state.registry)
// const actions = dispatch => bindActionCreators({ lookup }, dispatch)
// export default connect(model, actions)(RegistryWithData)

export default RegistryWithData