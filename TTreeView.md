# Using TTreeView in Lazarus

The TTreeView component in Lazarus is a standard component that allows you to create hierarchical lists. This guide will walk you through its usage and common operations.

## Adding TTreeView to Your Form

1. In the Lazarus IDE, go to the "Standard" tab of the component palette.
2. Find and double-click on TTreeView to add it to your form.

## Basic Usage in Code

First, ensure you have the necessary unit in your uses clause:

```pascal
uses
  ComCtrls; // Make sure this is in your uses clause
```  

Here's a basic example of adding nodes to the TTreeView:

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  RootNode, ChildNode: TTreeNode;
begin
  // Add a root node
  RootNode := TreeView1.Items.Add(nil, 'Root Node');
  
  // Add child nodes
  ChildNode := TreeView1.Items.AddChild(RootNode, 'Child 1');
  TreeView1.Items.AddChild(RootNode, 'Child 2');
  
  // Add a grandchild
  TreeView1.Items.AddChild(ChildNode, 'Grandchild');
  
  // Expand the root node
  RootNode.Expand(False);
end;
```

## Handling Node Selection

```pascal
procedure TForm1.TreeView1SelectionChanged(Sender: TObject);
begin
  if TreeView1.Selected <> nil then
    ShowMessage('Selected: ' + TreeView1.Selected.Text);
end;
```

## Adding Nodes Dynamically

```pascal
procedure TForm1.AddNodeButtonClick(Sender: TObject);
var
  NewNode: TTreeNode;
begin
  if TreeView1.Selected <> nil then
    NewNode := TreeView1.Items.AddChild(TreeView1.Selected, 'New Node')
  else
    NewNode := TreeView1.Items.Add(nil, 'New Root Node');
  TreeView1.Selected := NewNode;
end;
```

## Deleting Nodes

```pascal
procedure TForm1.DeleteNodeButtonClick(Sender: TObject);
begin
  if TreeView1.Selected <> nil then
    TreeView1.Items.Delete(TreeView1.Selected);
end;
```

## Customizing the Appearance

```pascal
TreeView1.ShowRoot := False; // Hide the root node
TreeView1.ShowLines := False; // Hide the lines connecting nodes
TreeView1.ReadOnly := True; // Make the tree read-only
```

## Working with Node Data

You can associate custom data with each node using the Data property:

```pascal
type
  PMyData = ^TMyData;
  TMyData = record
    ID: Integer;
    Description: string;
  end;

procedure TForm1.AddNodeWithData;
var
  NewNode: TTreeNode;
  NodeData: PMyData;
begin
  New(NodeData);
  NodeData^.ID := 1;
  NodeData^.Description := 'Some description';
  NewNode := TreeView1.Items.AddChild(nil, 'Node with Data');
  NewNode.Data := NodeData;
end;

// Don't forget to free the data when deleting nodes or closing the form
procedure TForm1.FreeNodeData(Node: TTreeNode);
begin
  if Node.Data <> nil then
    Dispose(PMyData(Node.Data));
end;
```

## Handling Keyboard Input

```pascal
procedure TForm1.TreeView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DELETE:
      if TreeView1.Selected <> nil then
        TreeView1.Items.Delete(TreeView1.Selected);
    VK_INSERT:
      AddNodeButtonClick(nil); // Reuse the add node method
  end;
end;
```

Remember to set up event handlers in the Object Inspector for events like OnSelectionChanged, OnKeyDown, etc., as needed for your application.

This guide provides a good starting point for working with TTreeView in Lazarus. You can further customize its behavior and appearance based on your specific requirements.
