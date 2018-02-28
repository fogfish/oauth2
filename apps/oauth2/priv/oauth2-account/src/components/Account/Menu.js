import React from 'react'
import { Tab, TabElement } from 'react-dress-code'

const Menu = () => (
  <Tab vertical>
    <TabElement header>Developer Settings</TabElement>
    <TabElement active>OAuth Apps</TabElement>
  </Tab>
)

export default Menu
