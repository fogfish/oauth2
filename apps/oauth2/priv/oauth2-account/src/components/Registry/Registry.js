import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { lifecycle } from 'recompose'
import { H2, H4, Card, Button } from 'react-dress-code'
import RegistryNone from './RegistryNone'
import RegistryList from './RegistryList'
import { lookup } from './ducks'

const Registry = (props) => (
  <Card>
    <H2>
      OAuth Apps 
      <Button link small onClick={ () => props.history.push('/oauth2/account/app') }>
        New OAuth App
      </Button>
    </H2>

    {props.apps.length == 0 ? <RegistryNone { ...props } /> : <RegistryList { ...props } />}
    
  </Card>
)

const RegistryWithData = lifecycle({
  componentWillMount() {
    this.props.lookup()
  }
})(Registry)


const model = state => (state.registry)
const actions = dispatch => bindActionCreators({ lookup }, dispatch)
export default connect(model, actions)(RegistryWithData)
