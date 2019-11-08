import React from 'react'
import { Navbar, Alignment, Icon, Tooltip, Intent } from '@blueprintjs/core'
import { IconNames } from '@blueprintjs/icons'
import { authorize, WhoIs } from 'components/OAuth2'

const Account = () => WhoIs(({ sub }) => (<>{sub}</>))

const Header = () => (
  <Navbar>
    <Navbar.Group align={Alignment.LEFT}>
      <Navbar.Heading>Account</Navbar.Heading>
      <Navbar.Divider />
      <Account />
    </Navbar.Group>
    <Navbar.Group align={Alignment.RIGHT}>
      <Tooltip intent={Intent.DANGER} content="Sign Out">
        <Icon iconSize={20} icon={IconNames.POWER} onClick={authorize} />
      </Tooltip>
    </Navbar.Group>
  </Navbar>
)

export default Header
