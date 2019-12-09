import React from 'react'
import { AnchorButton, Intent } from '@blueprintjs/core'

const GitHub = ({ clientId }) => (
  <AnchorButton
    intent={Intent.PRIMARY}
    minimal
    href={`${window.env.GITHUB}&state=${clientId}`}
  >
    <i className="fa fa-github" />
    &nbsp;&nbsp;GitHub
  </AnchorButton>
)

export default GitHub
