import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Table, THead, TBody, TR, TH, TD, Button } from 'react-dress-code'
import { revoke } from './ducks'

const Item = ({name, access, security, redirect_uri, revoke}) => (
  <TR key={name} interactive>
    <TD>{name}</TD>
    <TD>{access}</TD>
    <TD>{security}</TD>
    <TD>{redirect_uri}</TD>
    <TD>
      <Button link small remove onClick={() => revoke(access)}>revoke</Button>
    </TD>
  </TR>
)

const RegistryList = (props) => (
  <Table responsive>
    <THead>
      <TR>
        <TH>Application</TH>
        <TH>Client ID</TH>
        <TH>Security</TH>
        <TH>Redirect Uri</TH>
        <TH></TH>
      </TR>
    </THead>
    <TBody>
      {props.apps.map((x) => <Item {...x} revoke={props.revoke} />)}
    </TBody>
  </Table>
)

const model = state => (state.registry)
const actions = dispatch => bindActionCreators({ revoke }, dispatch)
export default connect(model, actions)(RegistryList)
