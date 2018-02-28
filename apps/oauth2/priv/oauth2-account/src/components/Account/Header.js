import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Tab, TabElement, Button } from 'react-dress-code'
import { authorize } from '../OAuth2/ducks'

const Header = ({ authorize }) => (
  <Tab>
    <TabElement header>
      Account
   </TabElement>
    <TabElement right>
      <Button link small onClick={authorize}>
         Sign Out
      </Button>
   </TabElement>
  </Tab>
)

const model = state => (state)
const actions = dispatch => bindActionCreators({ authorize }, dispatch)
export default connect(model, actions)(Header)
