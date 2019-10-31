import React from 'react'
import { Navbar, Alignment, Button, Intent } from '@blueprintjs/core'

import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { authorize } from '../OAuth2/ducks'

const Header = ({ authorize }) => (
  <Navbar>
    <Navbar.Group align={Alignment.LEFT}>
      <Navbar.Heading>Account</Navbar.Heading>
      <Navbar.Divider />
    </Navbar.Group>
    <Navbar.Group align={Alignment.RIGHT}>
      <Button minimal intent={Intent.PRIMARY} text="Sign Out" onClick={authorize} />
    </Navbar.Group>
  </Navbar>
)

const model = state => (state)
const actions = dispatch => bindActionCreators({ authorize }, dispatch)
export default connect(model, actions)(Header)
