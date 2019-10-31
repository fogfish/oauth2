import React from 'react'
import { Card, H2, Button, Intent } from '@blueprintjs/core'


import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { lifecycle } from 'recompose'
import { LoadingBar } from 'react-dress-code'
import RegistryNone from './RegistryNone'
import RegistryList from './RegistryList'
import { lookup } from './ducks'

const Registry = (props) => (
  <Card>
    <H2 style={{display: 'flex', justifyContent: 'space-between'}}>
      OAuth Apps 
      <Button minimal small intent={Intent.PRIMARY} onClick={ () => props.history.push('/oauth2/account/app') }>
        New OAuth App
      </Button>
    </H2>

    {!props.apps && <LoadingBar />}
    {(!props.apps || props.apps.length === 0) && <RegistryNone { ...props } />}
    {(props.apps  && props.apps.length  >  0) && <RegistryList { ...props } />}    
  </Card>
)

const RegistryWithData = lifecycle({
  componentWillMount() {
    if (!this.props.apps)
      this.props.lookup()
  }
})(Registry)


const model = state => (state.registry)
const actions = dispatch => bindActionCreators({ lookup }, dispatch)
export default connect(model, actions)(RegistryWithData)
